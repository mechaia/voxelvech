mod player;

use crate::{
    gfx::{Draw, DrawCollector, Gfx},
    gui::{self, ItemPicker},
    log,
    physics::Physics,
    scenario::World,
    vehicle::{self, Block, BlockSet, DamageAccumulator, Vehicle, VehicleSound},
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
                let mut snd = self.sounds.lock().unwrap();
                self.player_vehicle.apply_damage(
                    &mut self.physics,
                    &mut snd.player_vehicle,
                    player_dmg,
                );
                for ((v, snd), dmg) in self
                    .enemy_vehicles
                    .iter_mut()
                    .zip(snd.enemy_vehicles.iter_mut())
                    .zip(enemy_dmg)
                {
                    v.apply_damage(&mut self.physics, snd, dmg);
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
    fn step(&mut self, state: &mut crate::State) {
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
            snd.player_vehicle
                .update(&p_trf.apply_to_transform_inv(&v_trf));
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
                .push(state.block_set.mesh_set(), mesh as _, 1, &trfs)
        }

        crate::gui::draw_logbox(draw);
        match &self.ui {
            Ui::Nothing => gui::draw_crosshair(draw),
            Ui::Menu(p) | Ui::Load(p) | Ui::Spawn(p) => p.render(draw.viewport_rect(), draw),
            _ => {}
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

/*
struct Tone {
    start: U0d32,
    frequency: u32,
    amplitude: f32,
}

fn gen_cracking_tones<R: Rng>(rng: &mut R, tones: &mut Vec<Tone>, start: U0d32, cur_freq: f32) {
    if tones.len() >= 128 {
        return;
    }
    for _ in 0..4 {
        //let new_freq = cur_freq * rng.gen_range(2.0..=10.0);
        //let new_freq = cur_freq * rng.gen_range(1.5..=10.0);
        //let new_freq = cur_freq * rng.gen_range(1.5..=3.0);
        let new_freq = cur_freq * rng.gen_range(1.5..=4.0);
        //let new_freq = cur_freq * rng.gen_range(1.5..=10.0);
        //let new_freq = cur_freq * rng.gen_range(1.5..=5.0);
        if new_freq >= 2e4 {
            continue;
        }
        let start = rng.gen_range(start..=U0d32::MAX / 10);
        gen_cracking_tones(rng, tones, start, new_freq);
    }
    tones.push(Tone {
        start,
        frequency: cur_freq as _,
        //amplitude: 1.0 / cur_freq,
        amplitude: 1.0,
    });
}

fn new_sounds_handler(
    sounds: Sounds,
) -> (half::Half<half::Left, Mutex<Sounds>>, crate::SoundsHandler) {
    let (ret_sounds, sounds) = half::half(Mutex::new(sounds));

    let mut t0 = U0d32::ZERO;
    let mut t1 = U0d32::ZERO;
    let mut t2 = U0d32::ZERO;
    let mut t = U0d32::ZERO;
    let mut src = mechaia::sound::mix::wave::RandomStep2::new(rand::rngs::OsRng::default(), 1e3);
    let mut src = mechaia::sound::mix::wave::RandomStep::new(rand::rngs::OsRng::default(), 1e3);

    let new_params = move || {
        let mut rng = rand::thread_rng();
        let mut v = Vec::new();
        let f = rng.gen_range(200.0..=2000.0);
        let f = 200.0;
        gen_cracking_tones(&mut rng, &mut v, U0d32::ZERO, f);
        let s = v.iter().map(|x| x.amplitude).sum::<f32>();
        v.iter_mut().for_each(|x| x.amplitude /= s);
        let mut v = v.into_iter().map(|t| (Wrapping(U0d32::ZERO), t)).collect::<Vec<_>>();
        dbg!(v.len());
        v
    };
    let mut params = new_params();
    let mut trigger = false;

    let sounds_handler = Box::new(
        move |buf: &mut [f32], channels: u16, interpolate: RangeInclusive<f32>, dt: U0d32| {
            for w in buf.iter_mut() {
                if t < U0d32::from_f32(0.99) {
                    *w = params
                        .iter_mut()
                        .filter(|(_, tone)| U32d32::from(tone.start) <= U32d32::from(t) * 3)
                        .map(|(t, tone)| {
                            *t += dt.wrapping_mul_u32(tone.frequency);
                            let v = mechaia::sound::mix::wave::sine(t.0) * tone.amplitude;
                            let f = 5.0;
                            let f = 0.05;
                            tone.amplitude *= (-(tone.frequency as f32) * dt.to_f32() * f).exp();
                            v
                        })
                        .sum();
                    // *w = src.next_sample(dt.into());
                    // *w = mechaia::sound::mix::wave::triangle(t);
                    /*
                    *w = 0.0;
                    *w += mechaia::sound::mix::wave::sine(t0) * 0.4;
                    *w += mechaia::sound::mix::wave::sine(t1) * 0.3;
                    *w += mechaia::sound::mix::wave::sine(t2) * 0.3;
                    *w *= (-t.to_f32() * 20.0).exp();
                    t0 = t0.wrapping_add(dt.wrapping_mul_u32(200));
                    t1 = t1.wrapping_add(dt.wrapping_mul_u32(1000));
                    t2 = t2.wrapping_add(dt.wrapping_mul_u32(10000));
                    */
                    trigger = true;
                } else {
                    if trigger {
                        params = new_params();
                    }
                    trigger = false;
                }
                t = t.wrapping_add(dt);
            }
            return;

            {
                let mut snd = sounds.lock().unwrap();
                if snd.paused {
                    dbg!();
                    return;
                }
                snd.player_vehicle.next_samples(interpolate, dt, buf);
            }
            let mut i = buf.len() / usize::from(channels);
            while i > 0 {
                i -= 1;
                let k = i * usize::from(channels);
                let v = buf[i];
                buf[k..k + usize::from(channels)].fill(v);
            }
        },
    );

    (ret_sounds, sounds_handler)
}
*/

/*
struct Tone {
    t: Wrapping<U0d32>,
    frequency: u32,
    amplitude: f32,
    buildup: f32,
}

fn gen_tone<R: Rng>(rng: &mut R) -> Tone {
    Tone {
        t: Default::default(),
        //frequency: rng.gen_range(800..=12000),
        frequency: rng.gen_range(200..=12000),
        amplitude: 1.0,
        buildup: 0.0,
    }
}

fn gen_tone_freq(f: f32) -> Tone {
    Tone {
        t: Default::default(),
        frequency: f as u32,
        //amplitude: 1.0 / f,
        amplitude: 1e3 / f,
        buildup: 0.0,
    }
}

fn sample_tone(tone: &mut Tone, dt: U0d32) -> Option<f32> {
    if tone.amplitude < 1e-3 {
        return None
    }
    tone.buildup += dt.to_f32() * 20.0;
    tone.buildup = tone.buildup.min(1.0);
    tone.t += dt.wrapping_mul_u32(tone.frequency);
    let v = mechaia::sound::mix::wave::sine(tone.t.0);
    //let v = mechaia::sound::mix::wave::square(tone.t.0);
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1000.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 100.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 10.0).exp2();
    tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.2).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.1).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.05).exp2();
    Some(v * tone.amplitude * tone.buildup)
}

fn new_sounds_handler(
    sounds: Sounds,
) -> (half::Half<half::Left, Mutex<Sounds>>, crate::SoundsHandler) {
    let (ret_sounds, sounds) = half::half(Mutex::new(sounds));

    let mut tones = Vec::new();
    let mut t = U32d32::ZERO;
    let mut out = std::io::BufWriter::new(std::fs::OpenOptions::new().write(true).truncate(true).create(true).open("/tmp/test.f32.raw").unwrap());

    let mut queue = VecDeque::new();

    let sounds_handler = Box::new(
        move |buf: &mut [f32], channels: u16, interpolate: RangeInclusive<f32>, dt: U0d32| {
            //dbg!(t);
            /*
            if t >= U32d32::ONE * 10 {
                std::process::exit(0);
            }
            */
            let mut rng = rand::thread_rng();
            for w in buf.iter_mut() {
                //if rng.gen_bool(0.001) {
                //    tones.push(gen_tone(&mut rng));
                //}
                //let f = rng.gen_range(1e2..=1e4);
                //let f = rng.gen_range(2e3..=3e3);
                let f = rng.gen_range(4e2..=8e2);
                //let f = rng.gen_range(2e2..=3e2);
                //let f = rng.gen_range(2e3..=3e3);
                if rng.gen_bool((f / 1.5e5).into()) {
                //if rng.gen_bool((f / 1.5e4).into()) {
                //if rng.gen_bool((f / 1.5e6).into()) {
                //if rng.gen_bool((f / 1.5e6 * t.to_f32()).into()) {
                    tones.push(gen_tone_freq(f));
                }
                let mut vv = 0.0;
                for i in (0..tones.len()).rev() {
                    if let Some(v) = sample_tone(&mut tones[i], dt) {
                        vv += v;
                    } else {
                        tones.swap_remove(i);
                    }
                }
                vv *= 3.0;
                //vv /= 3.0;
                if queue.len() > 10 {
                    queue.pop_back();
                }
                queue.push_front(vv);
                *w = queue.iter().sum::<f32>() / queue.len() as f32;
                std::io::Write::write_all(&mut out, &w.to_le_bytes()).unwrap();
            }
            t += U32d32::from(dt) * buf.len() as u32;
        },
    );

    (ret_sounds, sounds_handler)
}
*/

/*
struct Tone {
    t: Wrapping<U0d32>,
    frequency: u32,
    amplitude: f32,
}

fn gen_tone<R: Rng>(rng: &mut R) -> Tone {
    Tone {
        t: Default::default(),
        //frequency: rng.gen_range(800..=12000),
        frequency: rng.gen_range(200..=12000),
        amplitude: 1.0,
    }
}

fn gen_tone_freq(f: f32) -> Tone {
    Tone {
        t: Default::default(),
        frequency: f as u32,
        amplitude: 1e3 / f,
    }
}

fn sample_tone(tone: &mut Tone, dt: U0d32) -> Option<f32> {
    if tone.amplitude < 1e-2 {
        return None
    }
    tone.t += dt.wrapping_mul_u32(tone.frequency);
    let v = mechaia::sound::mix::wave::sine(tone.t.0);
    //let v = mechaia::sound::mix::wave::square(tone.t.0);
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1000.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 100.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 10.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1.0).exp2();
    tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.2).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.1).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.05).exp2();
    Some(v * tone.amplitude)
}

fn new_sounds_handler(
    sounds: Sounds,
) -> (half::Half<half::Left, Mutex<Sounds>>, crate::SoundsHandler) {
    let (ret_sounds, sounds) = half::half(Mutex::new(sounds));

    let mut tones = Vec::new();
    let mut t = U32d32::ZERO;
    let mut trigger = false;
    let mut out = std::io::BufWriter::new(std::fs::OpenOptions::new().write(true).truncate(true).create(true).open("/tmp/test.f32.raw").unwrap());

    let sounds_handler = Box::new(
        move |buf: &mut [f32], channels: u16, interpolate: RangeInclusive<f32>, dt: U0d32| {
            /*
            if t >= U32d32::ONE * 5 {
                std::process::exit(0);
            }
            */
            let mut rng = rand::thread_rng();
            for w in buf.iter_mut() {
                //if rng.gen_bool(0.001) {
                //    tones.push(gen_tone(&mut rng));
                //}
                if tones.is_empty() && !trigger && t.frac().to_f32() < 0.1 {
                    for i in 0..4 {
                        tones.push(gen_tone_freq(rng.gen_range(200.0..=500.0)));
                    }
                    //trigger = true
                }
                for i in (0..tones.len()).rev() {
                    if let Some(v) = sample_tone(&mut tones[i], dt) {
                        *w += v;
                    } else {
                        tones.swap_remove(i);
                    }
                }
                if t.frac().to_f32() > 0.9 {
                    trigger = false
                }
                *w /= 10.0;
                *w *= (-t.frac().to_f32() * 20.0).exp();
                std::io::Write::write_all(&mut out, &w.to_le_bytes()).unwrap();
            }
            t += U32d32::from(dt) * buf.len() as u32;
        },
    );

    (ret_sounds, sounds_handler)
}
*/

/*
struct Tone {
    t: Wrapping<U0d32>,
    frequency: u32,
    amplitude: f32,
    buildup: f32,
}

fn gen_tone<R: Rng>(rng: &mut R) -> Tone {
    Tone {
        t: Default::default(),
        //frequency: rng.gen_range(800..=12000),
        frequency: rng.gen_range(200..=12000),
        amplitude: 1.0,
        buildup: 0.0,
    }
}

fn gen_tone_freq(f: f32) -> Tone {
    Tone {
        t: Default::default(),
        frequency: f as u32,
        //amplitude: 1.0 / f,
        amplitude: 1e3 / f,
        buildup: 0.0,
    }
}

fn sample_tone(tone: &mut Tone, dt: U0d32) -> Option<f32> {
    if tone.amplitude < 1e-3 {
        return None
    }
    tone.buildup += dt.to_f32() * 20.0;
    tone.buildup = tone.buildup.min(1.0);
    tone.t += dt.wrapping_mul_u32(tone.frequency);
    let v = mechaia::sound::mix::wave::sine(tone.t.0);
    //let v = mechaia::sound::mix::wave::square(tone.t.0);
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1000.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 100.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 10.0).exp2();
    tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.2).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.1).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.05).exp2();
    Some(v * tone.amplitude * tone.buildup)
}

fn new_sounds_handler(
    sounds: Sounds,
) -> (half::Half<half::Left, Mutex<Sounds>>, crate::SoundsHandler) {
    let (ret_sounds, sounds) = half::half(Mutex::new(sounds));

    let mut tones = Vec::new();
    let mut t = U32d32::ZERO;
    let mut out = std::io::BufWriter::new(std::fs::OpenOptions::new().write(true).truncate(true).create(true).open("/tmp/test.f32.raw").unwrap());

    let mut queue = VecDeque::new();

    let sounds_handler = Box::new(
        move |buf: &mut [f32], channels: u16, interpolate: RangeInclusive<f32>, dt: U0d32| {
            //dbg!(t);
            /*
            if t >= U32d32::ONE * 10 {
                std::process::exit(0);
            }
            */
            let mut rng = rand::thread_rng();
            for w in buf.iter_mut() {
                //if rng.gen_bool(0.001) {
                //    tones.push(gen_tone(&mut rng));
                //}
                //let f = rng.gen_range(1e2..=1e4);
                //let f = rng.gen_range(2e3..=3e3);
                let f = rng.gen_range(4e2..=8e2);
                //let f = rng.gen_range(2e2..=3e2);
                //let f = rng.gen_range(2e3..=3e3);
                if rng.gen_bool((f / 1.5e5).into()) {
                //if rng.gen_bool((f / 1.5e4).into()) {
                //if rng.gen_bool((f / 1.5e6).into()) {
                //if rng.gen_bool((f / 1.5e6 * t.to_f32()).into()) {
                    tones.push(gen_tone_freq(f));
                }
                let mut vv = 0.0;
                for i in (0..tones.len()).rev() {
                    if let Some(v) = sample_tone(&mut tones[i], dt) {
                        vv += v;
                    } else {
                        tones.swap_remove(i);
                    }
                }
                vv *= 3.0;
                //vv /= 3.0;
                if queue.len() > 10 {
                    queue.pop_back();
                }
                queue.push_front(vv);
                *w = queue.iter().sum::<f32>() / queue.len() as f32;
                std::io::Write::write_all(&mut out, &w.to_le_bytes()).unwrap();
            }
            t += U32d32::from(dt) * buf.len() as u32;
        },
    );

    (ret_sounds, sounds_handler)
}
*/

struct Tone {
    t: Wrapping<U0d32>,
    attack: U0d32,
    hold: U0d32,
    frequency: u32,
    amplitude: f32,
}

fn gen_tone_freq(t: U0d32, hold: U0d32, f: f32, amplitude: f32) -> Tone {
    Tone {
        t: Wrapping(t),
        attack: Default::default(),
        hold,
        frequency: f as u32,
        amplitude,
        //amplitude: 1e3 / f,
    }
}

fn sample_tone(tone: &mut Tone, dt: U0d32) -> Option<f32> {
    if tone.amplitude < 1e-2 {
        return None
    }
    tone.t += dt.wrapping_mul_u32(tone.frequency * 3);
    let v = mechaia::sound::mix::wave::sine(tone.t.0);
    //let v = mechaia::sound::mix::wave::square(tone.t.0);
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1000.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 100.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 10.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 2.0).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 1.0).exp2();
    /*
    tone.attack = tone.attack.saturating_add(dt);
    if tone.attack == U0d32::MAX {
        tone.amplitude *= (-dt.to_f32() * 1000.0).exp2();
    }
    */
    tone.hold = tone.hold.saturating_sub(dt);
    if tone.hold == U0d32::ZERO {
        tone.amplitude *= (-dt.to_f32() * 4e2).exp2();
    }
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.2).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.1).exp2();
    //tone.amplitude *= (-dt.to_f32() * tone.frequency as f32 / 0.05).exp2();
    Some(v * tone.amplitude)
}

fn new_sounds_handler(
    sounds: Sounds,
) -> (half::Half<half::Left, Mutex<Sounds>>, crate::SoundsHandler) {
    let (ret_sounds, sounds) = half::half(Mutex::new(sounds));

    let mut tones = Vec::new();
    let mut t = U32d32::ZERO;
    let mut trigger = false;
    let mut out = std::io::BufWriter::new(std::fs::OpenOptions::new().write(true).truncate(true).create(true).open("/tmp/test.f32.raw").unwrap());

    let sounds_handler = Box::new(
        move |buf: &mut [f32], channels: u16, interpolate: RangeInclusive<f32>, dt: U0d32| {
            /*
            if t >= U32d32::ONE * 5 {
                std::process::exit(0);
            }
            */
            let mut rng = rand::thread_rng();
            for w in buf.iter_mut() {
                //if rng.gen_bool(0.001) {
                //    tones.push(gen_tone(&mut rng));
                //}
                if tones.is_empty() && !trigger && t.frac().to_f32() < 0.1 {
                    /*¨
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 64, 436.363636363));
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 64, 1777.77777777));
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 64, 4363.63636363));
                    */
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 32, rng.gen_range(4e2..=4.5e2) * 1.0, 0.3));
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 32, rng.gen_range(5e2..=8e2) * 1.0, 0.3));
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 32, rng.gen_range(1.5e3..=2e3) * 1.0, 0.2));
                    tones.push(gen_tone_freq(Default::default(), U0d32::FRAC_1_4 / 32, rng.gen_range(4e3..=4.5e3) * 1.0, 0.2));
                    /*
                    tones.push(gen_tone_freq(rng.gen(), rng.gen_range(5e2..=1e3)));
                    tones.push(gen_tone_freq(rng.gen(), rng.gen_range(3e3..=4e3)));
                    tones.push(gen_tone_freq(rng.gen(), rng.gen_range(3e3..=4e3)));
                    tones.push(gen_tone_freq(rng.gen(), rng.gen_range(3e3..=4e3)));
                    */
                    trigger = true
                }
                for i in (0..tones.len()).rev() {
                    if let Some(v) = sample_tone(&mut tones[i], dt) {
                        *w += v;
                    } else {
                        tones.swap_remove(i);
                    }
                }
                if t.frac().to_f32() > 0.9 {
                    trigger = false
                }
                //*w *= 0.995 + rng.gen_range(0.0..=1.0) / 100.0;
                //*w = rng.gen_range(-1.0..=1.0) / 10.0;
                //*w *= (-t.frac().to_f32() * 20.0).exp();
                std::io::Write::write_all(&mut out, &w.to_le_bytes()).unwrap();
                t += U32d32::from(dt).wrapping_mul(U32d32::from(1));
            }
        },
    );

    (ret_sounds, sounds_handler)
}
