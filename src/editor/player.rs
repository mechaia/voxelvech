use crate::{
    camera::FreeCamera,
    physics::Physics,
    vehicle::{Block, BlockSet, BlockSetEntryData, DamageAccumulator, Vehicle},
};
use mechaia::{
    math::{IVec3, Vec3},
    render::resource::camera::CameraView,
    util::Transform,
};

pub struct Player {
    camera: FreeCamera,
    /// Rotations per block per direction.
    /// It feels nicer and more intuitive.
    block_rotations: Box<[[u8; 6]]>,
    /// Currently selected block.
    block_id: u32,
}

pub struct InputEffects {
    pub vehicle_dirty: bool,
    pub block_ghost: Option<(IVec3, Block)>,
}

impl Player {
    pub fn new(block_set: &BlockSet) -> Self {
        Self {
            camera: FreeCamera::new(),
            block_rotations: (0..block_set.len()).map(|_| [0; 6]).collect(),
            block_id: block_set.cube_id(),
        }
    }

    pub fn handle_inputs(
        &mut self,
        state: &crate::State,
        physics: &mut Physics,
        vehicle: &mut Vehicle,
        sounds: &mut super::Sounds,
    ) -> InputEffects {
        let input = |n| state.input.state.get_by_name(n);
        self.camera.add_pan(state.input("editor.camera.pan"));
        self.camera.add_tilt(state.input("editor.camera.tilt"));
        self.camera.translate(
            state.time.delta
                * Vec3::new(
                    state.input("editor.camera.forward"),
                    state.input("editor.camera.sideways"),
                    state.input("editor.camera.up"),
                ),
        );

        self.block_id = (self.block_id
            + u32::from(state.input_hold_or_edge_over("editor.select_block.toggle")))
            % state.block_set.len_u32();

        let mut vehicle_dirty = false;
        let mut block_ghost = None;

        let mut query = vehicle.query_ray(
            physics,
            self.camera.translation(),
            self.camera.direction(),
            false,
        );

        if let Some(q) = &query {
            if let Some(free) = q.free {
                let dir = self.direction(&state.block_set, q.occupied, free);
                let rot = &mut self.block_rotations[self.block_id as usize][usize::from(dir)];

                if is_wheel(&state.block_set, self.block_id) {
                    *rot &= 2;
                    if state.input_hold_or_edge_over("editor.rotate") {
                        *rot = (*rot).wrapping_add(2) & 2;
                    } else if state.input_hold_or_edge_under("editor.rotate") {
                        *rot = (*rot).wrapping_sub(2) & 2;
                    }
                    *rot |= 1;
                } else if is_core(&state.block_set, self.block_id) {
                    *rot = 0;
                } else {
                    if state.input_hold_or_edge_over("editor.rotate") {
                        *rot = (*rot).wrapping_add(1) & 3;
                    } else if state.input_hold_or_edge_under("editor.rotate") {
                        *rot = (*rot).wrapping_sub(1) & 3;
                    }
                }

                if state.input_hold_or_edge_over("editor.place") {
                    let blk = Block::new(self.block_id, dir, *rot);
                    vehicle.force_add(
                        physics,
                        &mut sounds.player_vehicle,
                        &state.block_set,
                        free,
                        blk,
                    );
                    vehicle_dirty = true;
                }
            }

            // avoid simultaneous place and remove, which would cause weird effects.
            if state.input_hold_or_edge_over("editor.remove") && !vehicle_dirty {
                vehicle.force_remove(physics, &mut sounds.player_vehicle, q.occupied);
                vehicle_dirty = true;
            }

            // recast ray if block got added or removed for visual consistency of ghost block
            if vehicle_dirty {
                query = vehicle.query_ray(
                    physics,
                    self.camera.translation(),
                    self.camera.direction(),
                    false,
                )
            }
        }

        if state.input_hold_or_edge_over("editor.deathray.heat") && !vehicle_dirty {
            let dmg_query = vehicle.query_ray(
                physics,
                self.camera.translation(),
                self.camera.direction(),
                true,
            );
            if let Some(q) = &dmg_query {
                let mut acc = DamageAccumulator::default();
                acc.add_heat(q.occupied.as_vec3(), Vec3::X, 260);
                vehicle.apply_damage(physics, &mut sounds.player_vehicle, &mut sounds.all_vehicles, acc);

                query = vehicle.query_ray(
                    physics,
                    self.camera.translation(),
                    self.camera.direction(),
                    false,
                )
            }
        }

        if let Some(q) = query {
            if let Some(free) = q.free {
                let dir = self.direction(&state.block_set, q.occupied, free);
                let rot = self.block_rotations[self.block_id as usize][usize::from(dir)];
                block_ghost = Some((free, Block::new(self.block_id, dir, rot)))
            }
        }

        InputEffects {
            vehicle_dirty,
            block_ghost,
        }
    }

    pub fn transform(&self) -> Transform {
        self.camera.transform()
    }

    pub fn camera_to_render(&self, aspect: f32) -> CameraView {
        self.camera.to_render(aspect)
    }

    pub fn looking_at_phys(&self, physics: &Physics) -> Vec3 {
        let max_distance = 1e4;
        let distance = physics
            .engine
            .cast_ray(
                self.camera.translation(),
                self.camera.direction() * max_distance,
                None,
            )
            .map_or(max_distance, |v| v.distance);
        self.camera.translation() + self.camera.direction() * distance
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
    matches!(&block_set[block_id].data, BlockSetEntryData::Wheel { .. })
}

fn is_core(block_set: &BlockSet, block_id: u32) -> bool {
    block_set[block_id].name.starts_with("core")
}
