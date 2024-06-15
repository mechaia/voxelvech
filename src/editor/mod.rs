mod player;

use crate::{
    gfx::{Draw, DrawCollector, Gfx},
    gui::{self, ItemPicker},
    log,
    physics::Physics,
    scenario::World,
    vehicle::{self, Block, BlockSet, DamageAccumulator, LaserSound, Vehicle, VehicleSound},
    Scenario,
};
use mechaia::{
    input::KeyOrName,
    math::{IVec3, Quat, Vec3},
    render::resource::camera::CameraView,
    util::{
        math::fixed::{U0d32, U32d32},
        rand::{self, Rng},
        sync::half,
        Transform,
    },
};
use std::{
    collections::VecDeque,
    io::{self, Write},
    num::Wrapping,
    ops::RangeInclusive,
    sync::Mutex,
    time::{Duration, Instant, SystemTime},
};

const MAX_PHYSICS_STEPS: u32 = 8;

pub struct Editor {
    physics: Physics,
    world: World,
    player_vehicle: Vehicle,
    enemy_vehicles: Vec<Vehicle>,
    projectiles: Vec<(crate::vehicle::Projectile, Instant)>,
    player: player::Player,
    autosaver: Autosaver,
    /// Use single time starting point for consistent interpolation and other effects.
    physics_step_start: Instant,
    block_ghost: Option<(IVec3, Block)>,
    ui: Ui,
    /// File of player vehicle to default to.
    player_vehicle_file: String,
    sounds: half::Half<half::Left, Mutex<Sounds>>,
}

struct Sounds {
    player_vehicle: VehicleSound,
    enemy_vehicles: Vec<VehicleSound>,
    all_vehicles: vehicle::GlobalSound,
    paused: bool,
}

struct Autosaver {
    next_save: u8,
    max_saves: u8,
    deadline: Option<Instant>,
    timeout_secs: u16,
}

enum Ui {
    Nothing,
    Menu(ItemPicker),
    Load(ItemPicker),
    Save(ItemPicker),
    Spawn(ItemPicker),
    Scenario(ItemPicker),
}

impl Autosaver {
    fn init(max_saves: u8, timeout_secs: u16) -> Self {
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
            deadline: None,
            timeout_secs,
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

    fn init_deadline(&mut self) {
        if self.deadline.is_none() {
            self.deadline =
                Instant::now().checked_add(Duration::from_secs(self.timeout_secs.into()));
        }
    }
}

impl Editor {
    const USER_DATA_NONE: u32 = 0;
    const USER_DATA_PLAYER: u32 = 1;
    const USER_DATA_ENEMY_START: u32 = 2;

    pub fn new(state: &mut crate::State, gfx: &mut Gfx) -> (Self, crate::SoundsHandler) {
        let mut physics = Physics::new();

        let world = World::load_from_asset("data/scenario_editor.glb", &mut physics, gfx)
            .expect("failed to load editor scene");

        let mut sounds = Sounds {
            player_vehicle: Default::default(),
            enemy_vehicles: Default::default(),
            all_vehicles: Default::default(),
            paused: false,
        };

        let player_vehicle = default_vehicle(
            &mut physics,
            &mut sounds.player_vehicle,
            &state.block_set,
            Self::USER_DATA_PLAYER,
        );
        let player = player::Player::new(&state.block_set);

        let (sounds, sounds_handler) = new_sounds_handler(sounds);

        let slf = Self {
            physics,
            world,
            player_vehicle,
            enemy_vehicles: Default::default(),
            projectiles: Default::default(),
            player,
            autosaver: Autosaver::init(3, 300),
            physics_step_start: state.time.now,
            block_ghost: None,
            ui: Ui::Nothing,
            player_vehicle_file: "".to_string(),
            sounds,
        };

        crate::log::debug("Editor initialized");

        (slf, sounds_handler)
    }

    fn set_ui(&mut self, ui: Ui) {
        self.ui = ui;
        self.sounds.lock().unwrap().paused = !matches!(&self.ui, Ui::Nothing);
    }

    fn handle_ui(&mut self, state: &mut crate::State) {
        if state.input_hold_or_edge_over("ui.menu") {
            self.set_ui(if matches!(&self.ui, Ui::Nothing) {
                Ui::Menu(ItemPicker {
                    items: [
                        "continue",
                        "reset vehicle",
                        "spawn enemy",
                        "remove enemies",
                        "scenario",
                        "save",
                        "load",
                        "exit",
                    ]
                    .map(|s| s.to_string().into_boxed_str())
                    .into(),
                    selected: 0,
                    filter: "".to_string(),
                })
            } else {
                Ui::Nothing
            });
        }

        match &mut self.ui {
            Ui::Nothing => {}
            Ui::Menu(picker) => {
                if let Some(item) = picker.handle_input(state) {
                    match item {
                        "continue" => self.set_ui(Ui::Nothing),
                        "save" => {
                            self.set_ui(Ui::Save(ItemPicker {
                                items: self.autosaver.list(),
                                selected: 0,
                                filter: self.player_vehicle_file.clone(),
                            }));
                        }
                        "load" => {
                            self.set_ui(Ui::Load(ItemPicker {
                                items: self.autosaver.list(),
                                selected: 0,
                                filter: "".to_string(),
                            }));
                        }
                        "spawn enemy" => {
                            self.set_ui(Ui::Spawn(ItemPicker {
                                items: self.autosaver.list(),
                                selected: 0,
                                filter: "".to_string(),
                            }));
                        }
                        "remove enemies" => {
                            for v in self.enemy_vehicles.drain(..) {
                                v.destroy(&mut self.physics);
                            }
                            log::success("enemies removed");
                            self.set_ui(Ui::Nothing);
                        }
                        "exit" => {
                            if self.autosaver.deadline.is_some() {
                                self.autosaver.save(&mut |f| {
                                    self.player_vehicle.save_v0_text(&state.block_set, f)
                                });
                            }
                            return;
                        }
                        "reset vehicle" => {
                            reset_vehicle_transform(&mut self.physics, &mut self.player_vehicle);
                            self.player_vehicle.reset_damage(&state.block_set);
                            self.set_ui(Ui::Nothing);
                        }
                        "scenario" => {
                            self.set_ui(Ui::Scenario(ItemPicker {
                                items: state.scenarios.keys().cloned().collect::<Vec<_>>(),
                                selected: 0,
                                filter: "".to_string(),
                            }));
                        }
                        s => todo!("{s}"),
                    }
                }
            }
            Ui::Load(picker) => {
                if let Some(item) = picker.handle_input(state) {
                    match std::fs::read_to_string(item) {
                        Ok(s) => {
                            self.player_vehicle.load_v0_text(
                                &mut self.sounds.lock().unwrap().player_vehicle,
                                &state.block_set,
                                &mut self.physics,
                                &s,
                            );
                            reset_vehicle_transform(&mut self.physics, &mut self.player_vehicle);
                            log::success("loaded vehicle");
                            self.set_ui(Ui::Nothing);
                            self.autosaver.deadline = None;
                        }
                        Err(e) => {
                            log::error(format!("error reading '{item}': {e}"));
                        }
                    };
                }
            }
            Ui::Save(picker) => {
                // TODO how do we handle *new* files?
                todo!();
                /*
                if let Some(item) = picker.handle_input(state) {
                    if !p.ends_with(".vvt") {
                        // avoid surprised, complete with extension and retry
                        state.gui.data.file_picker.input_extend(".vvt".chars());
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
                                        state.gui.data.show = None;
                                        self.autosaver.deadline = None;
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
                */
            }
            Ui::Spawn(picker) => {
                if let Some(item) = picker.handle_input(state) {
                    match std::fs::read_to_string(item) {
                        Ok(s) => {
                            let udata = (2 + self.enemy_vehicles.len()).try_into().unwrap();
                            let mut v_snd = VehicleSound::default();
                            let mut v = Vehicle::new_v0_text(
                                &state.block_set,
                                &mut self.physics,
                                &mut v_snd,
                                &s,
                                udata,
                            );
                            let pos = self.player.looking_at_phys(&self.physics);
                            v.set_transform(
                                &mut self.physics,
                                &Transform::IDENTITY.with_translation(pos + Vec3::Z * 10.0),
                            );
                            log::success("spawned enemy vehicle");
                            self.enemy_vehicles.push(v);
                            self.sounds.lock().unwrap().enemy_vehicles.push(v_snd);
                            self.set_ui(Ui::Nothing);
                        }
                        Err(e) => {
                            log::error(format!("error reading '{item}': {e}"));
                        }
                    };
                }
            }
            Ui::Scenario(picker) => {
                if let Some(item) = picker.handle_input(state) {
                    todo!();
                }
            }
        }
    }

    fn step_physics(&mut self, state: &crate::State) {
        if !matches!(&self.ui, Ui::Nothing) {
            self.physics_step_start = state.time.now;
            return;
        }

        let dt = Duration::from_secs_f32(self.physics.engine.time_delta());
        for _ in 0..MAX_PHYSICS_STEPS {
            let next_t = self.physics_step_start.checked_add(dt).unwrap();
            if next_t > state.time.now {
                return;
            }

            let mut player_dmg = DamageAccumulator::default();
            let mut enemy_dmg = self
                .enemy_vehicles
                .iter()
                .map(|_| DamageAccumulator::default())
                .collect::<Vec<_>>();

            let mut proj = self.player_vehicle.set_input_controls(
                state,
                &self.physics,
                &vehicle::InputControls {
                    forward: state.input.state.get_by_name("vehicle.control.forward"),
                    pan: state.input.state.get_by_name("vehicle.control.pan"),
                    tilt: 0.0,
                    target: self.player.looking_at_phys(&self.physics),
                    fire: state.input.state.get_by_name("vehicle.control.fire") > 0.0,
                },
            );

            for proj in proj.iter() {
                let start = proj.start.translation;
                let dir = proj.start.rotation * Vec3::Y;
                if let Some(hit) = self.physics.engine.cast_ray(start, dir * proj.length, None) {
                    match hit.user_data {
                        0 => {}
                        1 => {} // ignore player
                        i => {
                            let i = usize::try_from(i - 2).unwrap();
                            let (start, dir) =
                                self.enemy_vehicles[i].query_ray_params(&self.physics, start, dir);
                            enemy_dmg[i].add_heat(start, dir, 120);
                        }
                    }
                }
            }

            {
                let snd = &mut *self.sounds.lock().unwrap();
                self.player_vehicle.apply_damage(
                    &mut self.physics,
                    &mut snd.player_vehicle,
                    &mut snd.all_vehicles,
                    player_dmg,
                );
                for ((v, v_snd), dmg) in self
                    .enemy_vehicles
                    .iter_mut()
                    .zip(snd.enemy_vehicles.iter_mut())
                    .zip(enemy_dmg)
                {
                    v.apply_damage(&mut self.physics, v_snd, &mut snd.all_vehicles, dmg);
                }

                // sound stuff
                for p in proj.iter() {
                    snd.all_vehicles.lasers.push((
                        p.start.translation,
                        LaserSound {
                            pitch_decay: 40.0,
                            //dt_factor: U32d32::ONE * 10000,
                            dt_factor: U32d32::ONE * 4000,
                            volume: Default::default(),
                            t: Default::default(),
                            pan: crate::PanFactor(0.0), // FIXME
                        },
                    ));
                }
            }

            // visual stuff
            self.projectiles.extend(proj.into_iter().map(|v| {
                (
                    v,
                    state
                        .time
                        .now
                        .checked_add(Duration::from_millis(100))
                        .unwrap(),
                )
            }));

            self.player_vehicle.step(&mut self.physics);
            for v in self.enemy_vehicles.iter_mut() {
                v.step(&mut self.physics);
            }

            {
                let mut snd = self.sounds.lock().unwrap();
                self.player_vehicle.apply_sounds(&mut snd.player_vehicle);
                for (v, snd) in self
                    .enemy_vehicles
                    .iter_mut()
                    .zip(snd.enemy_vehicles.iter_mut())
                {
                    v.step(&mut self.physics);
                }
            }

            self.physics.engine.step();
            self.physics_step_start = next_t;
        }

        log::debug("can't keep up! skipping physics steps");
    }
}

impl Scenario for Editor {
    fn step(&mut self, state: &mut crate::State, gfx: &mut Gfx) {
        self.block_ghost = if matches!(&self.ui, Ui::Nothing) {
            let res = {
                let mut snd = self.sounds.lock().unwrap();
                self.player.handle_inputs(
                    &state,
                    &mut self.physics,
                    &mut self.player_vehicle,
                    &mut snd,
                )
            };
            if res.vehicle_dirty {
                self.autosaver.init_deadline();
            }
            res.block_ghost
        } else {
            None
        };

        self.handle_ui(state);

        if self.autosaver.deadline.is_some_and(|t| t <= state.time.now) {
            self.autosaver
                .save(&mut |f| self.player_vehicle.save_v0_text(&state.block_set, f));
            self.autosaver.deadline = None;
        }

        self.step_physics(state);

        {
            let mut snd = self.sounds.lock().unwrap();
            let v_trf = self.player_vehicle.transform(&mut self.physics);
            let p_trf = self.player.transform();
            let w2v = p_trf.inverse();
            snd.player_vehicle.update(&w2v.apply_to_transform(&v_trf));
            for v in snd.enemy_vehicles.iter_mut() {
                v.update(&w2v.apply_to_transform(&v_trf));
            }
            snd.all_vehicles.update(&w2v);
        }
    }

    fn draw(&mut self, state: &mut crate::State, draw: &mut Draw<'_>) {
        self.world.draw(draw);

        let time_frac = state
            .time
            .now
            .duration_since(self.physics_step_start)
            .as_secs_f32()
            / self.physics.engine.time_delta();
        // Avoid surprises like vehicles flying away when game is paused.
        let time_frac = time_frac.min(1.0);

        self.player_vehicle
            .render(&state, &self.physics, time_frac, draw);
        for v in self.enemy_vehicles.iter() {
            v.render(state, &self.physics, time_frac, draw);
        }

        if let Some((pos, blk)) = &self.block_ghost {
            let mesh = state.block_set[blk.id].mesh();
            let trf_count = state.collection.meshes[mesh as usize].transform_count;

            let trf = self.player_vehicle.transform(&self.physics);
            let trfs = (0..trf_count)
                .map(|_| mechaia::util::TransformScale {
                    translation: trf.apply_to_translation(pos.as_vec3()),
                    rotation: trf.rotation * blk.orientation(),
                    scale: 1.0,
                })
                .collect::<Vec<_>>();
            draw.collector
                .transparent
                .push(state.block_set.mesh_set(), mesh, 1, &trfs);
        }

        self.projectiles.retain(|(_, t)| *t > state.time.now);

        for (proj, _) in self.projectiles.iter() {
            let mesh = proj.mesh_index;
            let armature = &state.collection.armatures[proj.armature_index as usize];
            let tail_trf = Transform::IDENTITY;
            let head_trf = Transform::IDENTITY.with_translation(Vec3::Y * proj.length);

            let tail_trf = proj.start.apply_to_transform(&tail_trf);
            let head_trf = proj.start.apply_to_transform(&head_trf);

            // apply directly so we don't "overshoot"
            // the armature has a fixed length to keep things a bit simpler when modeling
            // we need to make sure we don't add this length.
            // easiest solution is to simply bypass the bone hierarchy
            let tail_trf = armature.apply_direct(0, &tail_trf);
            let head_trf = armature.apply_direct(1, &head_trf);

            let trfs = [tail_trf, head_trf].map(|x| x.into());
            draw.collector
                .solid
                .push(state.block_set.mesh_set(), mesh as _, 3, &trfs)
        }

        crate::gui::draw_logbox(draw);
        match &self.ui {
            Ui::Nothing => gui::draw_crosshair(draw),
            Ui::Menu(p) | Ui::Load(p) | Ui::Spawn(p) | Ui::Scenario(p) => {
                p.render(draw.viewport_rect(), draw)
            }
            Ui::Save(_) => todo!(),
        }
    }

    fn camera(&self, aspect: f32) -> CameraView {
        self.player.camera_to_render(aspect)
    }

    fn destroy(mut self: Box<Self>, state: &mut crate::State, gfx: &mut Gfx) {
        if self.autosaver.deadline.is_some() {
            self.autosaver
                .save(&mut |f| self.player_vehicle.save_v0_text(&state.block_set, f));
        }
        self.world.destroy(&mut self.physics, gfx);
    }
}

fn default_vehicle(
    physics: &mut Physics,
    sound: &mut VehicleSound,
    block_set: &BlockSet,
    user_data: u32,
) -> Vehicle {
    let mut vehicle = Vehicle::new(physics, user_data);

    vehicle.force_add(
        physics,
        sound,
        block_set,
        IVec3::ZERO,
        Block::new(block_set.cube_id(), 0, 0),
    );
    reset_vehicle_transform(physics, &mut vehicle);
    sound.update(&vehicle.transform(physics));

    vehicle
}

fn reset_vehicle_transform(physics: &mut Physics, vehicle: &mut Vehicle) {
    vehicle.set_transform(physics, &Transform::new(Vec3::Z * 0.5, Quat::IDENTITY));
}

fn new_sounds_handler(
    sounds: Sounds,
) -> (half::Half<half::Left, Mutex<Sounds>>, crate::SoundsHandler) {
    let (ret_sounds, sounds) = half::half(Mutex::new(sounds));

    let sounds_handler = Box::new(move |receptor: &mut crate::DualReceptor<'_>| {
        let mut snd = sounds.lock().unwrap();
        if snd.paused {
            return;
        }
        snd.player_vehicle.next_samples(receptor);
        for v in snd.enemy_vehicles.iter_mut() {
            v.next_samples(receptor);
        }
        snd.all_vehicles.next_samples(receptor);
    });

    (ret_sounds, sounds_handler)
}
