mod camera;
mod data;
mod editor;
mod gfx;
mod gui;
mod log;
mod physics;
mod scenario;
mod vehicle;

use std::time::Instant;

use crate::{physics::Physics, scenario::Scenario, vehicle::BlockSet};
use mechaia::{
    input::{self, InputMap, InputState, KeyOrName},
    model::Collection,
    render::resource::camera::CameraView,
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

    let mut scenario = Box::new(editor::Editor::new(&mut state, &mut gfx));

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
        scenario.step(&mut state);

        let camera = scenario.camera(window.aspect());

        gfx.draw(&camera, &mut |draw| {
            scenario.draw(&mut state, draw);
        });
    }

    scenario.destroy(&mut state, &mut gfx);
}
