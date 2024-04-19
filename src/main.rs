mod camera;
mod gfx;
mod physics;
mod player;
mod time;
mod vehicle;

use core::iter;
use mechaia::{
    math::{EulerRot, IVec3, Quat, UVec2, Vec2, Vec3},
    physics3d,
    input,
};
use std::time::{Duration, Instant};

use crate::physics::Physics;

fn load_blocks() -> mechaia::model::Collection {
    mechaia::model::gltf::from_glb_slice(include_bytes!("../data/basicblocks.glb"))
}

const DEFAULT_INPUT_MAP: &str = include_str!("../data/inputmap_azerty.txt");

fn main() {
    let mut physics = Physics::new();
    let mut vehicle = vehicle::Vehicle::new(&mut physics);

    vehicle.force_add(&mut physics, IVec3::ZERO, vehicle::Block::new(1, 0, 0));

    let inputs = {
        let cfg = DEFAULT_INPUT_MAP;
        input::InputMap::parse_from_lines_graceful(cfg.lines(), &mut |i, msg| {
            dbg!(i, msg);
        })
    };

    let mut window = mechaia::window::Window::new();
    let mut gfx = gfx::Gfx::new(&mut window);

    let mut camera = camera::FreeCamera::new();

    let mut watch = time::StopWatch::new();

    let mut trigger_place = input::TriggerEdge::default();
    let mut trigger_remove = input::TriggerEdge::default();
    let mut trigger_toggle = input::TriggerEdge::default();
    let mut trigger_rotate = input::TriggerEdge::default();
    let mut trigger_mirror_enable = input::TriggerEdge::default();

    let mut block = vehicle::Block::new(0, 0, 0);
    let block_count = load_blocks().meshes.len() as u8;

    // I'm a fucking genius
    let mut rot_per_dir = [0u8; 6];

    let mut physics_watch = time::StopWatch::new();

    loop {
        let deadline = Instant::now()
            .checked_add(Duration::from_secs_f32(1.0 / 120.0))
            .unwrap();
        window.reset_mouse_relative();
        loop {
            let events = window.wait(deadline);
            if events.is_empty() {
                break;
            }
            for evt in events {
                use mechaia::window::Event;
                match evt {
                    Event::Resized(_) => gfx.render.rebuild_swapchain(),
                    Event::Input(_) => {}
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

        block.id =
            (block.id + trigger_toggle.apply(f("editor.select_block.toggle")) as u8) % block_count;

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
                }
            } else if trigger_remove.apply(f("editor.remove")) {
                vehicle.force_remove(&mut physics, q.occupied.local);
            }
            query = vehicle.query_ray(&physics, camera.translation(), camera.direction());
        }

        gfx.draw(&camera.to_render(window.aspect()), &mut |collector| {
            vehicle.render(&physics, collector);
            eprint!("\r");
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
                eprint!("{}", q.world);
            }
            eprint!("                   ");
            physics.render(collector);
        });

        if physics_watch.delta_now() >= physics.engine.time_delta() {
            vehicle.set_input_controls(&vehicle::InputControls {
                forward: f("vehicle.control.forward"),
                pan: f("vehicle.control.pan"),
                tilt: 0.0,
            });
            physics_watch.sample();
            vehicle.step(&mut physics);
            physics.engine.step();
        }
    }
}
