mod camera;
mod gfx;
mod log;
mod physics;
mod player;
mod time;
mod vehicle;

use core::iter;
use mechaia::{
    input,
    math::{EulerRot, IVec3, Quat, UVec2, Vec2, Vec3},
    physics3d,
};
use std::{
    collections::HashMap,
    io::{self, Write},
    time::{Duration, Instant, SystemTime},
};

use crate::physics::Physics;

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
            Err(e) => {
                return log::error(format!(
                    "failed to move {tmp_path} to {move_to}: {e}"
                ))
            }
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
                log::warn("filename with non-Unicde characters. Skipping");
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

fn main() {
    let collection = load_collection();

    let mut block_set = vehicle::BlockSet::from_collection(&collection);

    let mut physics = Physics::new(&collection);
    let mut vehicle = vehicle::Vehicle::new(&mut physics);

    vehicle.force_add(
        &mut physics,
        IVec3::ZERO,
        vehicle::Block::new(block_set.cube_id(), 0, 0),
    );

    let inputs = {
        let cfg = DEFAULT_INPUT_MAP;
        input::InputMap::parse_from_lines_graceful(cfg.lines(), &mut |i, msg| {
            let line = i + 1;
            log::error(format!("inputmap: {line} {msg}"));
        })
    };

    let mut window = mechaia::window::Window::new();
    let mut gfx = gfx::Gfx::new(&mut window, &collection);

    let mut camera = camera::FreeCamera::new();

    let mut watch = time::StopWatch::new();

    let mut trigger_place = input::TriggerEdge::default();
    let mut trigger_remove = input::TriggerEdge::default();
    let mut trigger_toggle = input::TriggerEdge::default();
    let mut trigger_rotate = input::TriggerEdge::default();
    let mut trigger_mirror_enable = input::TriggerEdge::default();

    let mut trigger_toggle_menu = input::TriggerEdge::default();
    let mut trigger_esc = input::TriggerEdge::default();

    let mut trigger_ui_up = input::TriggerEdge::default();
    let mut trigger_ui_down = input::TriggerEdge::default();
    let mut trigger_ui_select = input::TriggerEdge::default();
    let mut trigger_ui_complete = input::TriggerEdge::default();

    let mut block = vehicle::Block::new(block_set.cube_id(), 0, 0);

    // I'm a fucking genius
    let mut rot_per_dir = [0u8; 6];

    let mut physics_watch = time::StopWatch::new();

    let mut save_timeout: Option<time::StopWatch> = None;
    let save_timeout_thresh = 60.0;

    let mut autosaver = Autosaver::init(3);

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

        if gfx.gui.data.show.is_none() {
            camera.add_pan(f("editor.camera.pan"));
            camera.add_tilt(f("editor.camera.tilt"));
            camera.translate(
                watch.delta()
                    * Vec3::new(
                        f("editor.camera.forward"),
                        f("editor.camera.sideways"),
                        f("editor.camera.up"),
                    ),
            );

            block.id = (block.id + u32::from(trigger_toggle.apply(f("editor.select_block.toggle"))))
                % block_set.len_u32();

            let rot = &mut rot_per_dir[usize::from(block.direction())];
            if block.id == 4 {
                *rot &= 2;
                if trigger_rotate.apply(f("editor.rotate")) {
                    *rot = (*rot).wrapping_add(2) & 2;
                } else if trigger_rotate.apply(-f("editor.rotate")) {
                    *rot = (*rot).wrapping_sub(2) & 2;
                }
                *rot |= 1;
            } else {
                if trigger_rotate.apply(f("editor.rotate")) {
                    *rot = (*rot).wrapping_add(1) & 3;
                } else if trigger_rotate.apply(-f("editor.rotate")) {
                    *rot = (*rot).wrapping_sub(1) & 3;
                }
            }
            block.set_rotation(*rot);
        }

        let mut query = vehicle.query_ray(&physics, camera.translation(), camera.direction());

        if let Some(q) = query {
            if let Some(free) = &q.free {
                if block.id == 4 {
                    block.set_direction(0);
                } else {
                    block.set_direction(match (free.local - q.occupied.local).to_array() {
                        [0, 0, 1] => 0,
                        [0, 0, _] => 1,
                        [0, 1, _] => 2,
                        [0, _, _] => 3,
                        [1, _, _] => 4,
                        [_, _, _] => 5,
                    });
                }
            }

            if trigger_place.apply(f("editor.place")) {
                if let Some(free) = q.free {
                    vehicle.force_add(&mut physics, free.local, block.clone());
                    save_timeout.get_or_insert_with(time::StopWatch::new);
                }
            } else if trigger_remove.apply(f("editor.remove")) {
                vehicle.force_remove(&mut physics, q.occupied.local);
                save_timeout.get_or_insert_with(time::StopWatch::new);
            }
            query = vehicle.query_ray(&physics, camera.translation(), camera.direction());
        }

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
                    mechaia::window::InputKey::Unicode(c) => for c in c.chars() { fp.input_push(c) },
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
                        "exit" => {
                            if save_timeout.is_some() {
                                autosaver.save(&mut |f| vehicle.save_v0_text(&block_set, f));
                            }
                            return;
                        }
                        s => todo!("{s}"),
                    }
                }
            }
            Some(gfx::GuiShow::LoadList) => {
                handle_file_picker();
                if trigger_ui_select.apply(f("ui.select")) {
                    let p = gfx.gui.data.file_picker.input();
                    match std::fs::read_to_string(p.to_string()) {
                        Ok(s) => {
                            vehicle.load_v0_text(&block_set, &mut physics, &s);
                            vehicle.set_transform(&mut physics, &mechaia::physics3d::Transform::IDENTITY);
                            log::success("loaded vehicle");
                            gfx.gui.data.show = None;
                            save_timeout = None;
                        }
                        Err(e) => {
                            log::error(format!("error reading {p}: {e}"));
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
                                        log::success(format!("saved vehicle to {p}"));
                                        gfx.gui.data.show = None;
                                        save_timeout = None;
                                    }
                                    Err(e) => {
                                        log::error(format!("error writing {p}: {e}"));
                                    }
                                }
                            }
                            Err(e) => {
                                log::error(format!("error opening {p}: {e}"));
                            }
                        };
                    }
                }
            }
        }

        gfx.draw(&camera.to_render(window.aspect()), &mut |collector| {
            let time_frac = physics_watch.delta_now() / physics.engine.time_delta();
            vehicle.render(&block_set, &physics, time_frac, collector);
            if let Some(q) = query.as_ref().and_then(|q| q.free.as_ref()) {
                let trf = vehicle.transform(&physics);
                let trfs = [
                    mechaia::util::Transform {
                        translation: q.world,
                        rotation: trf.rotation * block.orientation(),
                        scale: 1.0,
                    },
                    mechaia::util::Transform {
                        translation: q.world,
                        rotation: trf.rotation * block.orientation(),
                        scale: 1.0,
                    },
                    mechaia::util::Transform {
                        translation: q.world,
                        rotation: trf.rotation * block.orientation(),
                        scale: 1.0,
                    },
                ];
                collector.transparent.push(block.id.into(), 1, &trfs);
            }
            physics.render(collector);
        });

        if physics_watch.delta_now() >= physics.engine.time_delta() {
            if gfx.gui.data.show.is_none() {
                if f("editor.vehicle.reset") != 0.0 {
                    vehicle.set_transform(
                        &mut physics,
                        &mechaia::physics3d::Transform {
                            translation: Vec3::ZERO,
                            rotation: Quat::IDENTITY,
                        },
                    );
                }

                vehicle.set_input_controls(
                    &physics,
                    &vehicle::InputControls {
                        forward: f("vehicle.control.forward"),
                        pan: f("vehicle.control.pan"),
                        tilt: 0.0,
                    },
                );
            }
            physics_watch.sample();
            vehicle.step(&mut physics);
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
