#![deny(unreachable_patterns)]

mod camera;
mod data;
mod editor;
mod gfx;
mod gui;
mod log;
mod physics;
mod scenario;
mod vehicle;

use crate::{
    physics::Physics,
    scenario::{Scenario, ScenarioInfo},
    vehicle::BlockSet,
};
use mechaia::{
    input::{self, InputMap, InputState, KeyOrName},
    math::Vec3,
    model::Collection,
    util::math::fixed::{U0d32, U32d32},
};
use std::{
    collections::BTreeMap,
    ops::RangeInclusive,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

const DEFAULT_INPUT_MAP: &str = include_str!("../data/inputmap_azerty.txt");

struct State {
    collection: Collection,
    block_set: BlockSet,
    input: Input,
    time: Time,
    scenarios: BTreeMap<Box<str>, ScenarioInfo>,
}

struct Input {
    map: InputMap,
    state: InputState,
}

struct Time {
    prev: Instant,
    now: Instant,
    delta: f32,
}

struct Sound {
    dev: mechaia::sound::Dev,
}

type SoundsHandler = Box<dyn FnMut(&mut DualReceptor<'_>) + Send>;

struct SoundShared {
    f: SoundsHandler,
    prev_time: Instant,
    time: Instant,
    prev_interpolate: f32,
    pan_factor: f32,
}

struct DualReceptor<'a> {
    buf: &'a mut [f32],
    pan_factor: f32,
    channels: u16,
    dt: U0d32,
    df: f32,
    start_f: f32,
}

#[derive(Clone, Copy, Default)]
struct PanFactor(f32);

impl State {
    fn input(&self, name: &str) -> f32 {
        self.input.state.get_by_name(name)
    }

    fn input_hold_or_edge_over(&self, name: &str) -> bool {
        self.input.state.events().any(|e| match &e.key {
            KeyOrName::Name(n) if &**n == name => e.is_edge_or_hold_over(0.5),
            _ => false,
        })
    }

    fn input_hold_or_edge_under(&self, name: &str) -> bool {
        self.input.state.events().any(|e| match &e.key {
            KeyOrName::Name(n) if &**n == name => e.is_edge_or_hold_under(-0.5),
            _ => false,
        })
    }
}

impl<'a> DualReceptor<'a> {
    fn add(&mut self, pan: PanFactor, mut f: impl FnMut(U0d32, f32) -> f32) {
        for (i, w) in self.buf.chunks_mut(self.channels.into()).enumerate() {
            let ff = self.start_f + (self.df * i as f32);
            let v = f(self.dt, ff.clamp(0.0, 1.0));
            match w {
                [a] => *a += v,
                [a, b] => {
                    let gv = v * (1.0 - self.pan_factor);
                    let pv = v * self.pan_factor;
                    *a += gv + (pv * (1.0 - pan.0));
                    *b += gv + (pv * pan.0);
                }
                _ => todo!("more than 2 channels"),
            }
        }
    }
}

impl PanFactor {
    pub fn from_dir(dir: Vec3) -> Self {
        let pan = dir.normalize().dot(Vec3::Y);
        Self((pan * 0.5) + 0.5)
    }
}

fn main() {
    let mut window = mechaia::window::Window::new();
    let mut gfx = gfx::Gfx::new(&mut window);

    let time = Instant::now();
    let sound_data = Arc::new(Mutex::new(SoundShared {
        f: Box::new(|_| {}),
        prev_time: time,
        time,
        prev_interpolate: 1.0,
        //pan_factor: 0.8,
        pan_factor: 1.0,
    }));
    let mut sound = {
        let sound_data = sound_data.clone();

        let f = move |buf: &mut [f32], channels: u16, dt: U0d32| {
            buf.fill(0.0);
            let mut data = sound_data.lock().unwrap();

            let interp_t = Instant::now();
            let interp_dt = interp_t.duration_since(data.time);
            let total_dt = data.time.duration_since(data.prev_time);
            let interpolate = (interp_dt.as_secs_f32() / total_dt.as_secs_f32()).min(1.0);
            let interpolate_range = data.prev_interpolate..=interpolate;
            data.time = interp_t;
            data.prev_interpolate = interpolate;

            let receptor = &mut DualReceptor {
                pan_factor: data.pan_factor,
                channels,
                dt,
                start_f: data.prev_interpolate,
                df: (interpolate - data.prev_interpolate) / buf.len() as f32,
                buf,
            };

            (data.f)(receptor);
        };

        Sound {
            dev: mechaia::sound::Dev::new(
                &mechaia::sound::DevConfig {
                    //buffer_size: mechaia::sound::BufferSize::MaxLatency(U0d32::MAX / 100), // 10ms
                    buffer_size: mechaia::sound::BufferSize::MaxLatency(U0d32::MAX / 50), // 20ms
                    //buffer_size: mechaia::sound::BufferSize::Fixed(4096),
                    channels: 2,
                    sample_rate: 48000,
                },
                f,
            ),
        }
    };

    let collection = data::read_asset_gltf("data/basicblocks.glb").expect("");
    let block_set = BlockSet::from_collection(&mut gfx, &collection);
    let inputs = {
        let cfg = DEFAULT_INPUT_MAP;
        input::InputMap::parse_from_lines_graceful(cfg.lines(), &mut |i, msg| {
            let line = i + 1;
            log::error(format!("inputmap: {line} {msg}"));
        })
    };

    let prev @ now = Instant::now();

    let mut state = State {
        collection,
        block_set,
        input: Input {
            map: inputs,
            state: Default::default(),
        },
        time: Time {
            prev,
            now,
            delta: 0.0,
        },
        scenarios: Default::default(),
    };

    let mut scenario: Box<dyn Scenario>;
    {
        let mut args = std::env::args().skip(1);

        (scenario, sound_data.lock().unwrap().f) = match args.next().as_deref() {
            Some("scenario") => {
                let script = args.next().unwrap();
                let (a, b) = scenario::pil::PilScenario::new(&script, &mut state, &mut gfx);
                (Box::new(a) as Box<dyn Scenario>, b)
            }
            Some(s) => todo!("{s:?}"),
            None => {
                let (a, b) = editor::Editor::new(&mut state, &mut gfx);
                (Box::new(a) as Box<dyn Scenario>, b)
            }
        };
    };

    'lp: loop {
        state.input.state.clear_events();
        window.reset_mouse_relative();

        let events = window.wait(std::time::Instant::now());
        for evt in events {
            use mechaia::window::Event;
            match evt {
                Event::Resized(_) => gfx.rebuild(),
                Event::Input(inp) => state.input.state.push_key(inp.key, inp.value),
                Event::CloseRequested => break 'lp,
            }
        }

        // process named inputs in a separate pass to ensure all keys entered
        // this frame have been processed.
        //
        // If a named input is mapped to two distinct keys it may generate two events
        // in the same frame. This is consistent with the behavior where two events
        // occur in two consecutive frames, so keep it.
        let mut extend = Vec::new();
        for evt in state.input.state.events() {
            let KeyOrName::Key(key) = &evt.key else {
                continue;
            };
            for name in state.input.map.get_by_key(key) {
                let v = state
                    .input
                    .map
                    .get_by_name(name)
                    //.flat_map(|(k, r)| r.apply(state.input.state.get_by_key(k)))
                    .flat_map(|(k, r)| r.apply(window.input(k)))
                    .sum();
                extend.push((name, v));
            }
        }
        for (name, v) in extend {
            state.input.state.push_name(name.into(), v);
        }

        state.time.prev = state.time.now;
        state.time.now = Instant::now();
        state.time.delta = state.time.now.duration_since(state.time.prev).as_secs_f32();
        {
            let mut data = sound_data.lock().unwrap();
            data.prev_time = data.time;
            data.time = state.time.now;
            data.prev_interpolate = 0.0;
        }
        scenario.step(&mut state, &mut gfx);

        let camera = scenario.camera(window.aspect());

        gfx.draw(&camera, &mut |draw| {
            scenario.draw(&mut state, draw);
        });
    }

    scenario.destroy(&mut state, &mut gfx);
    sound.dev.pause();
}
