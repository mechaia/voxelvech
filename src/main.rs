mod camera;
mod data;
mod editor;
mod gfx;
mod gui;
mod log;
mod physics;
mod scenario;
mod vehicle;

use std::{
    ops::RangeInclusive,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use crate::{physics::Physics, scenario::Scenario, vehicle::BlockSet};
use mechaia::{
    input::{self, InputMap, InputState, KeyOrName},
    model::Collection,
    util::math::fixed::{U0d32, U32d32},
};

const DEFAULT_INPUT_MAP: &str = include_str!("../data/inputmap_azerty.txt");

struct State {
    collection: Collection,
    block_set: BlockSet,
    input: Input,
    time: Time,
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
    sample_fill_target: usize,
    t: U32d32,
}

type SoundsHandler = Box<dyn FnMut(&mut [f32], u16, RangeInclusive<f32>, U0d32) + Send>;

struct SoundShared {
    f: SoundsHandler,
    prev_time: Instant,
    time: Instant,
    prev_interpolate: f32,
}

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

fn main() {
    let mut window = mechaia::window::Window::new();
    let mut gfx = gfx::Gfx::new(&mut window);

    let time = Instant::now();
    let sound_data = Arc::new(Mutex::new(SoundShared {
        f: Box::new(|data, _, _, _| data.fill(0.0)),
        prev_time: time,
        time,
        prev_interpolate: 1.0,
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

            (data.f)(buf, channels, interpolate_range, dt);
        };

        Sound {
            dev: mechaia::sound::Dev::new(
                &mechaia::sound::DevConfig {
                    //buffer_size: mechaia::sound::BufferSize::MaxLatency(U0d32::MAX / 100), // 10ms
                    buffer_size: mechaia::sound::BufferSize::Fixed(4096),
                    //channels: 2,
                    channels: 1,
                    sample_rate: 48000,
                },
                f,
            ),
            t: U32d32::ZERO,
            //buffer_fill_target: 48000 / 100,
            //buffer_fill_target: 4096,
            //buffer_fill_target: 1024,
            sample_fill_target: 512 * 2,
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
    };

    let (scenario, sound_handler) = editor::Editor::new(&mut state, &mut gfx);
    let mut scenario = Box::new(scenario);
    sound_data.lock().unwrap().f = sound_handler;

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
        scenario.step(&mut state);

        let camera = scenario.camera(window.aspect());

        gfx.draw(&camera, &mut |draw| {
            scenario.draw(&mut state, draw);
        });
    }

    scenario.destroy(&mut state, &mut gfx);
    sound.dev.pause();
}
