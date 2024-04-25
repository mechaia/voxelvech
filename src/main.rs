mod camera;
mod gfx;
mod log;
mod physics;
mod player;
mod time;
mod vehicle;

use mechaia::{
    input,
    math::{IVec3, Quat, Vec3},
    util::Transform,
};
use std::{
    collections::VecDeque,
    io::{self, Write},
    time::{Duration, Instant, SystemTime},
};
use vehicle::BlockSet;

use crate::{
    physics::Physics,
    vehicle::{DamageAccumulator, Vehicle},
};

const DEFAULT_INPUT_MAP: &str = include_str!("../data/inputmap_azerty.txt");

fn load_collection() -> mechaia::model::Collection {
    mechaia::model::gltf::from_glb_slice(include_bytes!("../data/basicblocks.glb"))
}

struct Autosaver {
    next_save: u8,
    max_saves: u8,
}

impl Autosaver {
    fn init(max_saves: u8) -> Self {
        // check which file to save to next
        let mut next_save = 0;
        let mut next_save_age = SystemTime::now();
        for i in 0..max_saves {
            let path = format!("_autosave{i}.vvt");
            let meta = match std::fs::metadata(path) {
                Ok(m) => m,
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                    // no file, save to it next
                    next_save = i;
                    break;
                }
                // TODO log error
                Err(_) => continue,
            };
            let Ok(time) = meta.modified().or_else(|_| meta.created()) else {
                continue;
            };
            if time < next_save_age {
                next_save = i;
                next_save_age = time;
            }
        }
        Self {
            next_save,
            max_saves,
        }
    }

    fn save(
        &mut self,
        data: &mut dyn FnMut(&mut dyn FnMut(&[u8]) -> io::Result<()>) -> io::Result<()>,
    ) {
        // write to temp file and then move over other file to avoid corruption
        let tmp_path = "_autosave_temp.vvt";
        let f = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(tmp_path);
        let mut f = match f {
            Ok(f) => f,
            Err(e) => return log::error(format!("failed to open {tmp_path}: {e}")),
        };
        match data(&mut |s| f.write_all(s)) {
            Ok(()) => {}
            Err(e) => return log::error(format!("failed to write to {tmp_path}: {e}")),
        }
        let move_to = format!("_autosave{}.vvt", self.next_save);
        match std::fs::rename(tmp_path, &move_to) {
            Ok(()) => {}
            Err(e) => return log::error(format!("failed to move {tmp_path} to {move_to}: {e}")),
        }
        log::info(format!("saved to {move_to}"));
        self.next_save = (self.next_save + 1) % self.max_saves;
    }

    fn list(&self) -> Vec<Box<str>> {
        let mut v = Vec::new();
        for f in std::fs::read_dir(".").expect("failed to read dir") {
            let f = f.expect("failed to read dir entry");
            if !f.metadata().expect("metadata").is_file() {
                continue;
            }
            let name = f.file_name();
            let Some(name) = name.to_str() else {
                log::warn("filename with non-Unicode characters. Skipping");
                continue;
            };
            if !name.ends_with(".vvt") {
                continue;
            }
            v.push(name.into());
        }
        v
    }
}

fn default_vehicle(
    physics: &mut Physics,
    block_set: &BlockSet,
    user_data: u32,
) -> vehicle::Vehicle {
    let mut vehicle = vehicle::Vehicle::new(physics, user_data);

    vehicle.force_add(
        physics,
        block_set,
        IVec3::ZERO,
        vehicle::Block::new(block_set.cube_id(), 0, 0),
    );

    vehicle
}

fn main() {
    let collection = load_collection();

    let projectile_model = 'a: {
        for root in collection.scenes.iter() {
            for node in root.descendants() {
                if let Some(name) = node
                    .properties()
                    .name
                    .as_ref()
                    .filter(|n| &***n == "projectile.laser0")
                {
                    let mechaia::model::Node::Leaf { model, .. } = node else {
                        panic!("todo: handle multi-mesh blocks (or not?)");
                    };
                    break 'a *model;
                }
            }
        }
        panic!("no projectile model");
    };
    assert_ne!(projectile_model, usize::MAX);

    let block_set = BlockSet::from_collection(&collection);

    let mut physics = Physics::new(&collection);

    let mut vehicle = default_vehicle(&mut physics, &block_set, 1);
    let mut enemy_vehicles = Vec::<Vehicle>::new();

    let inputs = {
        let cfg = DEFAULT_INPUT_MAP;
        input::InputMap::parse_from_lines_graceful(cfg.lines(), &mut |i, msg| {
            let line = i + 1;
            log::error(format!("inputmap: {line} {msg}"));
        })
    };

    let mut window = mechaia::window::Window::new();
    let mut gfx = gfx::Gfx::new(&mut window, &collection);

    let mut player = player::Player::new(&block_set);

    let mut watch = time::StopWatch::new();

    let mut trigger_esc = input::TriggerEdge::default();

    let mut trigger_ui_up = input::TriggerEdge::default();
    let mut trigger_ui_down = input::TriggerEdge::default();
    let mut trigger_ui_select = input::TriggerEdge::default();
    let mut trigger_ui_complete = input::TriggerEdge::default();

    let mut physics_watch = time::StopWatch::new();

    let mut save_timeout: Option<time::StopWatch> = None;
    let save_timeout_thresh = 60.0;

    let mut autosaver = Autosaver::init(3);

    let mut projectiles = VecDeque::new();

    log::info("System online");

    loop {
        let deadline = Instant::now()
            .checked_add(Duration::from_secs_f32(1.0 / 120.0))
            .unwrap();
        window.reset_mouse_relative();
        let mut now_inputs = Vec::new();
        loop {
            let events = window.wait(deadline);
            if events.is_empty() {
                break;
            }
            for evt in events {
                use mechaia::window::Event;
                match evt {
                    Event::Resized(_) => gfx.render.rebuild_swapchain(),
                    Event::Input(inp) if inp.value > 0.0 => now_inputs.push(inp.key),
                    Event::Input(_) => {}
                    Event::CloseRequested => {
                        if save_timeout.is_some() {
                            autosaver.save(&mut |f| vehicle.save_v0_text(&block_set, f));
                        }
                        return;
                    }
                }
            }
        }

        watch.sample();

        let f = |k| {
            inputs
                .get(k)
                .iter()
                .flat_map(|(i, s)| s.apply(window.input(i)))
                .sum()
        };

        let block_ghost = if gfx.gui.data.show.is_none() {
            let res = player.handle_inputs(
                &block_set,
                &mut physics,
                &mut vehicle,
                &inputs,
                &window,
                watch.delta(),
            );
            if res.vehicle_dirty {
                save_timeout.get_or_insert_with(time::StopWatch::new);
            }
            res.block_ghost
        } else {
            None
        };

        if trigger_esc.apply(f("ui.toggle")) {
            if gfx.gui.data.show.is_some() {
                gfx.gui.data.show = None;
            } else {
                gfx.gui.data.show = Some(gfx::GuiShow::Menu);
                gfx.gui.data.menu.reset_index();
            }
        }

        let handle_file_picker = || {
            let fp = &mut gfx.gui.data.file_picker;

            for inp in now_inputs {
                match inp {
                    mechaia::window::InputKey::Unicode(c) if c.as_str() == "\t" => {
                        fp.input_search_or_complete();
                    }
                    mechaia::window::InputKey::Unicode(c) if c.as_str() == "\n" => {}
                    mechaia::window::InputKey::Unicode(c) => {
                        for c in c.chars() {
                            fp.input_push(c)
                        }
                    }
                    mechaia::window::InputKey::Backspace => fp.input_pop(),
                    _ => {}
                }
            }

            if trigger_ui_up.apply(f("ui.up")) {
                fp.prev_index()
            }
            if trigger_ui_down.apply(f("ui.down")) {
                fp.next_index()
            }
            if trigger_ui_complete.apply(f("ui.complete")) {
                fp.input_set_to_index();
            }
        };

        match &gfx.gui.data.show {
            None => {}
            Some(gfx::GuiShow::Menu) => {
                if trigger_ui_up.apply(f("ui.up")) {
                    gfx.gui.data.menu.prev_index()
                }
                if trigger_ui_down.apply(f("ui.down")) {
                    gfx.gui.data.menu.next_index()
                }
                if trigger_ui_select.apply(f("ui.select")) {
                    match gfx.gui.data.menu.current() {
                        "continue" => gfx.gui.data.show = None,
                        "save" => {
                            gfx.gui.data.show = Some(gfx::GuiShow::SaveList);
                            gfx.gui.data.file_picker.set_files(autosaver.list());
                        }
                        "load" => {
                            gfx.gui.data.show = Some(gfx::GuiShow::LoadList);
                            gfx.gui.data.file_picker.set_files(autosaver.list());
                        }
                        "spawn enemy" => {
                            gfx.gui.data.show = Some(gfx::GuiShow::SpawnEnemy);
                            gfx.gui.data.file_picker.set_files(autosaver.list());
                        }
                        "remove enemies" => {
                            for v in enemy_vehicles.drain(..) {
                                v.destroy(&mut physics);
                            }
                            log::success("enemies removed");
                            gfx.gui.data.show = None;
                        }
                        "exit" => {
                            if save_timeout.is_some() {
                                autosaver.save(&mut |f| vehicle.save_v0_text(&block_set, f));
                            }
                            return;
                        }
                        "reset vehicle" => {
                            vehicle.set_transform(&mut physics, &Transform::IDENTITY);
                            vehicle.reset_damage(&block_set);
                            gfx.gui.data.show = None;
                        }
                        s => todo!("{s}"),
                    }
                }
            }
            Some(gfx::GuiShow::LoadList) => {
                handle_file_picker();
                if trigger_ui_select.apply(f("ui.select")) {
                    let mut p = gfx.gui.data.file_picker.input();
                    if p.is_empty() {
                        p = gfx.gui.data.file_picker.selected().unwrap_or("");
                    }
                    let p = p.to_string();
                    match std::fs::read_to_string(&p) {
                        Ok(s) => {
                            vehicle.load_v0_text(&block_set, &mut physics, &s);
                            vehicle.set_transform(&mut physics, &Transform::IDENTITY);
                            log::success("loaded vehicle");
                            gfx.gui.data.show = None;
                            save_timeout = None;
                            // set to loaded file, might differ if we special-cased to selected()
                            gfx.gui.data.file_picker.input_clear();
                            gfx.gui.data.file_picker.input_extend(p.chars());
                        }
                        Err(e) => {
                            log::error(format!("error reading '{p}': {e}"));
                        }
                    };
                }
            }
            Some(gfx::GuiShow::SaveList) => {
                handle_file_picker();
                if trigger_ui_select.apply(f("ui.select")) {
                    let p = gfx.gui.data.file_picker.input();
                    if !p.ends_with(".vvt") {
                        // avoid surprised, complete with extension and retry
                        gfx.gui.data.file_picker.input_extend(".vvt".chars());
                    } else {
                        let f = std::fs::OpenOptions::new()
                            .write(true)
                            .truncate(true)
                            .create(true)
                            .open(p);
                        match f {
                            Ok(mut f) => {
                                match vehicle.save_v0_text(&block_set, &mut |b| f.write_all(b)) {
                                    Ok(()) => {
                                        log::success(format!("saved vehicle to '{p}'"));
                                        gfx.gui.data.show = None;
                                        save_timeout = None;
                                    }
                                    Err(e) => {
                                        log::error(format!("error writing '{p}': {e}"));
                                    }
                                }
                            }
                            Err(e) => {
                                log::error(format!("error opening '{p}': {e}"));
                            }
                        };
                    }
                }
            }
            Some(gfx::GuiShow::SpawnEnemy) => {
                handle_file_picker();
                if trigger_ui_select.apply(f("ui.select")) {
                    let mut p = gfx.gui.data.file_picker.input();
                    if p.is_empty() {
                        p = gfx.gui.data.file_picker.selected().unwrap_or("");
                    }
                    let p = p.to_string();
                    match std::fs::read_to_string(&p) {
                        Ok(s) => {
                            let udata = (2 + enemy_vehicles.len()).try_into().unwrap();
                            let mut v = Vehicle::new_v0_text(&block_set, &mut physics, &s, udata);
                            let pos = player.looking_at_phys(&physics);
                            v.set_transform(
                                &mut physics,
                                &Transform::IDENTITY.with_translation(pos + Vec3::Z * 10.0),
                            );
                            log::success("spawned enemy vehicle");
                            enemy_vehicles.push(v);
                            gfx.gui.data.show = None;
                        }
                        Err(e) => {
                            log::error(format!("error reading '{p}': {e}"));
                        }
                    };
                }
            }
        }

        {
            let t = Instant::now();
            while projectiles.front().is_some_and(|(_, pt)| pt <= &t) {
                projectiles.pop_front();
            }
        }

        let camera = player.camera_to_render(window.aspect());
        gfx.draw(&camera, &mut |collector| {
            let time_frac = physics_watch.delta_now() / physics.engine.time_delta();

            vehicle.render(&block_set, &collection, &physics, time_frac, collector);
            for v in enemy_vehicles.iter() {
                v.render(&block_set, &collection, &physics, time_frac, collector);
            }

            if let Some((pos, blk)) = &block_ghost {
                let mesh = block_set.get_mesh(blk.id);
                let trf_count = collection.meshes[mesh as usize].transform_count;

                let trf = vehicle.transform(&physics);
                let trfs = (0..trf_count)
                    .map(|_| mechaia::util::TransformScale {
                        translation: trf.apply_to_translation(pos.as_vec3()),
                        rotation: trf.rotation * blk.orientation(),
                        scale: 1.0,
                    })
                    .collect::<Vec<_>>();
                collector.transparent.push(mesh, 1, &trfs);
            }
            physics.render(collector);

            for ((trf, len), _) in projectiles.iter() {
                let trf: &Transform = trf;
                let len: &f32 = len;

                let model = &collection.models[projectile_model as usize];
                let mesh = model.mesh_index;
                let armature = &collection.armatures[model.armature_index];
                let tail_trf = Transform::IDENTITY;
                let head_trf = Transform::IDENTITY.with_translation(Vec3::Y * *len);

                let tail_trf = trf.apply_to_transform(&tail_trf);
                let head_trf = trf.apply_to_transform(&head_trf);

                // apply directly so we don't "overshoot"
                // the armature has a fixed length to keep things a bit simpler when modeling
                // we need to make sure we don't add this length.
                // easiest solution is to simply bypass the bone hierarchy
                let tail_trf = armature.apply_direct(0, &tail_trf);
                let head_trf = armature.apply_direct(1, &head_trf);

                let trfs = [tail_trf, head_trf].map(|x| x.into());
                collector.solid.push(mesh as _, 1, &trfs)
            }
        });

        if physics_watch.delta_now() >= physics.engine.time_delta() {
            let t = Instant::now()
                .checked_add(Duration::from_millis(100))
                .unwrap();
            physics_watch.sample();

            let mut player_dmg = DamageAccumulator::default();
            let mut enemy_dmg = enemy_vehicles
                .iter()
                .map(|_| DamageAccumulator::default())
                .collect::<Vec<_>>();

            if gfx.gui.data.show.is_none() {
                let mut proj = vehicle.set_input_controls(
                    &block_set,
                    projectile_model as u32,
                    &collection,
                    &physics,
                    &vehicle::InputControls {
                        forward: f("vehicle.control.forward"),
                        pan: f("vehicle.control.pan"),
                        tilt: 0.0,
                        target: player.looking_at_phys(&physics),
                        fire: f("vehicle.control.fire") > 0.0,
                    },
                );

                for (trf, len) in proj.iter() {
                    let start = trf.translation;
                    let dir = trf.rotation * Vec3::Y;
                    if let Some(hit) = physics.engine.cast_ray(start, dir * *len, None) {
                        match hit.user_data {
                            0 => {}
                            1 => {} // ignore player
                            i => {
                                let i = usize::try_from(i - 2).unwrap();
                                let (start, dir) =
                                    enemy_vehicles[i].query_ray_params(&physics, start, dir);
                                enemy_dmg[i].add_heat(start, dir, 120);
                            }
                        }
                    }
                }

                vehicle.apply_damage(&mut physics, player_dmg);
                for (v, dmg) in enemy_vehicles.iter_mut().zip(enemy_dmg) {
                    v.apply_damage(&mut physics, dmg);
                }

                // visual stuff
                projectiles.extend(proj.into_iter().map(|p| (p, t)));
            }

            vehicle.step(&mut physics);
            for v in enemy_vehicles.iter_mut() {
                v.step(&mut physics);
            }

            physics.engine.step();
        }

        if save_timeout
            .as_ref()
            .is_some_and(|t| t.delta_now() >= save_timeout_thresh)
        {
            autosaver.save(&mut |f| vehicle.save_v0_text(&block_set, f));
            save_timeout = None;
        }
    }
}
