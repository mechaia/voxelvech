use crate::{
    camera::FreeCamera,
    physics::Physics,
    vehicle::{Block, BlockSet, DamageAccumulator, Vehicle},
};
use mechaia::{
    input::{InputMap, TriggerEdge},
    math::{IVec3, Vec3},
    render::resource::camera::CameraView,
    window::Window,
};

pub struct Player {
    mode: Mode,
    /// Rotations per block per direction.
    /// It feels nicer and more intuitive.
    block_rotations: Box<[[u8; 6]]>,
    /// Currently selected block.
    block_id: u32,
    triggers: Triggers,
}

enum Mode {
    Free { camera: FreeCamera },
}

#[derive(Default)]
struct Triggers {
    toggle: TriggerEdge,
    rotate: TriggerEdge,
    place: TriggerEdge,
    remove: TriggerEdge,
    deathray_heat: TriggerEdge,
}

pub struct InputEffects {
    pub vehicle_dirty: bool,
    pub block_ghost: Option<(IVec3, Block)>,
}

impl Player {
    pub fn new(block_set: &BlockSet) -> Self {
        Self {
            mode: Mode::Free {
                camera: FreeCamera::new(),
            },
            block_rotations: (0..block_set.len()).map(|_| [0; 6]).collect(),
            block_id: block_set.cube_id(),
            triggers: Triggers::default(),
        }
    }

    pub fn handle_inputs(
        &mut self,
        block_set: &BlockSet,
        physics: &mut Physics,
        vehicle: &mut Vehicle,
        inputs: &InputMap,
        window: &Window,
        delta_time: f32,
    ) -> InputEffects {
        let f = |k| {
            inputs
                .get(k)
                .iter()
                .flat_map(|(i, s)| s.apply(window.input(i)))
                .sum()
        };

        let Mode::Free { camera } = &mut self.mode;

        camera.add_pan(f("editor.camera.pan"));
        camera.add_tilt(f("editor.camera.tilt"));
        camera.translate(
            delta_time
                * Vec3::new(
                    f("editor.camera.forward"),
                    f("editor.camera.sideways"),
                    f("editor.camera.up"),
                ),
        );

        self.block_id = (self.block_id
            + u32::from(self.triggers.toggle.apply(f("editor.select_block.toggle"))))
            % block_set.len_u32();

        let mut vehicle_dirty = false;
        let mut block_ghost = None;

        let mut query = vehicle.query_ray(physics, camera.translation(), camera.direction(), false);

        if let Some(q) = &query {
            if let Some(free) = q.free {
                let dir = self.direction(block_set, q.occupied, free);
                let rot = &mut self.block_rotations[self.block_id as usize][usize::from(dir)];

                // TODO should be a property of the block itself
                if is_wheel(block_set, self.block_id) {
                    *rot &= 2;
                    if self.triggers.rotate.apply(f("editor.rotate")) {
                        *rot = (*rot).wrapping_add(2) & 2;
                    } else if self.triggers.rotate.apply(-f("editor.rotate")) {
                        *rot = (*rot).wrapping_sub(2) & 2;
                    }
                    *rot |= 1;
                } else if is_core(block_set, self.block_id) {
                    *rot = 0;
                } else {
                    if self.triggers.rotate.apply(f("editor.rotate")) {
                        *rot = (*rot).wrapping_add(1) & 3;
                    } else if self.triggers.rotate.apply(-f("editor.rotate")) {
                        *rot = (*rot).wrapping_sub(1) & 3;
                    }
                }

                if self.triggers.place.apply(f("editor.place")) {
                    let blk = Block::new(self.block_id, dir, *rot);
                    vehicle.force_add(physics, block_set, free, blk);
                    vehicle_dirty = true;
                }
            }

            // avoid simultaneous place and remove, which would cause weird effects.
            if self.triggers.remove.apply(f("editor.remove")) && !vehicle_dirty {
                vehicle.force_remove(physics, q.occupied);
                vehicle_dirty = true;
            }

            // recast ray if block got added or removed for visual consistency of ghost block
            if vehicle_dirty {
                let Mode::Free { camera } = &self.mode;
                query = vehicle.query_ray(physics, camera.translation(), camera.direction(), false);
            }
        }

        if self.triggers.deathray_heat.apply(f("editor.deathray.heat")) && !vehicle_dirty {
            let Mode::Free { camera } = &self.mode;
            let dmg_query = vehicle.query_ray(physics, camera.translation(), camera.direction(), true);
            if let Some(q) = &dmg_query {
                let mut acc = DamageAccumulator::default();
                acc.add_heat(q.occupied, 260);
                vehicle.apply_damage(acc);

                let Mode::Free { camera } = &self.mode;
                query = vehicle.query_ray(physics, camera.translation(), camera.direction(), false);
            }
        }

        if let Some(q) = query {
            if let Some(free) = q.free {
                let dir = self.direction(block_set, q.occupied, free);
                let rot = self.block_rotations[self.block_id as usize][usize::from(dir)];
                block_ghost = Some((free, Block::new(self.block_id, dir, rot)))
            }
        }

        InputEffects {
            vehicle_dirty,
            block_ghost,
        }
    }

    pub fn camera_to_render(&self, aspect: f32) -> CameraView {
        let Mode::Free { camera } = &self.mode;
        camera.to_render(aspect)
    }

    pub fn looking_at_phys(&self, physics: &Physics) -> Vec3 {
        let Mode::Free { camera } = &self.mode;
        let max_distance = 1e4;
        let distance = physics
            .engine
            .cast_ray(
                camera.translation(),
                camera.direction() * max_distance,
                None,
            )
            .map_or(max_distance, |v| v.distance);
        camera.translation() + camera.direction() * distance
    }

    fn direction(&self, block_set: &BlockSet, occupied: IVec3, free: IVec3) -> u8 {
        match (free - occupied).to_array() {
            _ if is_wheel(block_set, self.block_id) => 0,
            _ if is_core(block_set, self.block_id) => 0,
            [0, 0, 1] => 0,
            [0, 0, _] => 1,
            [0, 1, _] => 2,
            [0, _, _] => 3,
            [1, _, _] => 4,
            [_, _, _] => 5,
        }
    }
}

fn is_wheel(block_set: &BlockSet, block_id: u32) -> bool {
    block_set.get_name(block_id).starts_with("wheel")
}

fn is_core(block_set: &BlockSet, block_id: u32) -> bool {
    block_set.get_name(block_id).starts_with("core")
}
