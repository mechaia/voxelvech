mod camera;
mod gfx;
mod input;
mod time;
mod voxel;

use mechaia::{
    math::{EulerRot, IVec3, Quat, UVec2, Vec2, Vec3},
    physics3d,
};
use std::time::{Duration, Instant};

fn load_blocks() -> Vec<mechaia::render::Mesh> {
    mechaia::render::Mesh::from_glb_slice(include_bytes!("../data/basicblocks.glb"))
}

mod defaults {
    pub mod inputmap {
        use mechaia::window::InputKey::{self, *};
        pub const INPUTMAP_AZERTY: &[(&str, InputKey, f32)] = &[
            ("editor.camera.forward", Z, 10.0),
            ("editor.camera.forward", S, -10.0),
            ("editor.camera.sideways", Q, -10.0),
            ("editor.camera.sideways", D, 10.0),
            ("editor.camera.up", Space, 10.0),
            ("editor.camera.up", LCtrl, -10.0),
            ("editor.camera.pan", MouseRelativeX, -0.01),
            ("editor.camera.tilt", MouseRelativeY, -0.01),
            ("editor.select_block.toggle", E, 1.0),
            ("editor.select_block.side", MouseRelativeY, -0.01),
            ("editor.select_block.up", MouseRelativeX, -0.01),
            ("editor.place", MouseButtonL, 1.0),
            ("editor.remove", MouseButtonR, 1.0),
            ("editor.rotate", MouseWheelY, 1.0),
            ("editor.mirror.x.enable", M, 1.0),
            ("editor.mirror.x.shift", PageUp, 1.0),
            ("editor.mirror.x.shift", PageDown, -1.0),
        ];
    }
}

#[derive(Clone)]
struct Block {
    id: u8,
    orientation: u8,
}

impl Block {
    const MIRROR_X: u8 = 0;
    const MIRROR_Y: u8 = 1;
    const MIRROR_Z: u8 = 2;

    fn direction(&self) -> u8 {
        self.orientation & 7
    }

    fn rotation(&self) -> u8 {
        self.orientation >> 3
    }

    fn set_direction(&mut self, direction: u8) {
        debug_assert!(direction < 6);
        self.orientation &= !7;
        self.orientation |= direction;
    }

    fn set_rotation(&mut self, rotation: u8) {
        debug_assert!(rotation < 4);
        self.orientation &= 7;
        self.orientation |= rotation << 3;
    }

    fn orientation(&self) -> Quat {
        use core::f32::consts::{FRAC_PI_2, PI};
        let r = match self.direction() {
            0 => Quat::IDENTITY,
            1 => Quat::from_rotation_x(PI),
            2 => Quat::from_rotation_x(-FRAC_PI_2),
            3 => Quat::from_rotation_x(FRAC_PI_2),
            4 => Quat::from_rotation_y(FRAC_PI_2),
            5 => Quat::from_rotation_y(-FRAC_PI_2),
            _ => unreachable!(),
        };
        r * Quat::from_rotation_z(FRAC_PI_2 * f32::from(self.rotation()))
    }
}

fn main() {
    let inputs = input::InputMap::from_list(defaults::inputmap::INPUTMAP_AZERTY);

    let mut window = mechaia::window::Window::new();
    let mut gfx = gfx::Gfx::new(&mut window);

    let mut camera = camera::FreeCamera::new();

    let mut watch = time::StopWatch::new();

    let mut voxels = voxel::Map::<Block>::new();

    let mut trigger_place = input::TriggerEdge::default();
    let mut trigger_remove = input::TriggerEdge::default();
    let mut trigger_toggle = input::TriggerEdge::default();
    let mut trigger_rotate = input::TriggerEdge::default();
    let mut trigger_mirror_enable = input::TriggerEdge::default();

    let mut block = Block {
        id: 0,
        orientation: 0,
    };
    let block_count = load_blocks().len() as u8;

    // I'm a fucking genius
    let mut rot_per_dir = [0u8; 6];

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
                .map(|&(i, s)| window.input(i) * s)
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

        let f_place_pos = |voxels: &voxel::Map<_>, camera: &camera::FreeCamera| {
            let ray = voxel::query::Ray::new(camera.translation(), camera.direction());
            let mut prev @ mut ppos = ray.current();
            for pos in ray.take(1024) {
                ppos = pos;
                if pos.z < 0 || voxels.get(pos).is_some() {
                    break;
                }
                prev = pos;
            }
            (ppos, prev)
        };

        block.id =
            (block.id + trigger_toggle.apply(f("editor.select_block.toggle")) as u8) % block_count;

        let (remove_pos, place_pos) = f_place_pos(&voxels, &camera);

        block.set_direction(match (place_pos - remove_pos).to_array() {
            [0, 0, 1] => 0,
            [0, 0, _] => 1,
            [0, 1, _] => 2,
            [0, _, _] => 3,
            [1, _, _] => 4,
            [_, _, _] => 5,
        });

        let rot = &mut rot_per_dir[usize::from(block.direction())];
        if trigger_rotate.apply(f("editor.rotate")) {
            *rot = (*rot).wrapping_add(1) & 3;
        } else if trigger_rotate.apply(-f("editor.rotate")) {
            *rot = (*rot).wrapping_sub(1) & 3;
        }
        block.set_rotation(*rot);

        if trigger_place.apply(f("editor.place")) {
            voxels.insert(place_pos, block.clone());
        } else if trigger_remove.apply(f("editor.remove")) {
            voxels.remove(remove_pos);
        }

        let (_, place_pos) = f_place_pos(&voxels, &camera);

        print!(
            "\r{}, {} -> {}                     ",
            camera.translation(),
            camera.direction(),
            place_pos
        );

        let len = u32::try_from(voxels.len()).unwrap();

        let f = &mut |index| {
            use mechaia::render::stage::standard3d::Instance;
            let it = |id| {
                voxels
                    .iter()
                    .filter(move |(_, blk)| blk.id == id)
                    .map(|(loc, blk)| Instance {
                        translation: loc.as_vec3() + Vec3::ONE / 2.0,
                        rotation: blk.orientation(),
                        material: 0,
                    })
            };

            let f = |id| u32::try_from(it(id).count()).unwrap();
            gfx.pbr_solid
                .set_instance_data(index, &(0..block_count).map(f).collect::<Vec<_>>(), &mut (0..block_count).flat_map(it));

            let f = |i| u32::from(block.id == i);
            gfx.pbr_transparent.set_instance_data(
                index,
                &(0..block_count).map(f).collect::<Vec<_>>(),
                &mut [Instance {
                    translation: place_pos.as_vec3() + Vec3::ONE / 2.0,
                    rotation: block.orientation(),
                    material: 1,
                }]
                .into_iter(),
            );
            gfx.gui.draw(
                index,
                &mut [mechaia::gui::Instance {
                    position: Vec2::ZERO,
                    half_extents: Vec2::ONE / 100.0,
                    rotation: 0.0,
                    uv_start: Vec2::ZERO,
                    uv_end: Vec2::ONE,
                    texture: 0,
                }]
                .into_iter(),
            );
        };

        gfx.render
            .draw(&camera.to_render(window.aspect()), gfx.stage_set, f);
    }
}
