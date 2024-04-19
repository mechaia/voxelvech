mod physics;

use core::iter;
use mechaia::{
    math::{EulerRot, IVec3, Quat, UVec2, Vec2, Vec3},
    physics3d, voxel,
};
use std::time::{Duration, Instant};

use crate::{
    gfx::{DrawCollector, Gfx},
    physics::Physics,
};

pub struct Vehicle {
    physics: physics::Body,
    voxels: voxel::common::Map<Block>,
}

#[derive(Default)]
pub struct InputControls {
    pub forward: f32,
    pub pan: f32,
    pub tilt: f32,
}

impl Vehicle {
    pub fn new(physics: &mut Physics) -> Self {
        Self {
            physics: physics::Body::new(physics),
            voxels: voxel::common::Map::new(),
        }
    }

    /// Add a block without any sanity checks.
    pub fn force_add(&mut self, physics: &mut Physics, pos: IVec3, block: Block) {
        self.physics.add(physics, pos, &block);
        self.voxels.insert(pos, block);
    }

    /// Remove a block without any sanity checks.
    pub fn force_remove(&mut self, physics: &mut Physics, pos: IVec3) {
        self.physics.remove(physics, pos);
        self.voxels.remove(pos);
    }

    pub fn query_ray(
        &mut self,
        physics: &Physics,
        start: Vec3,
        direction: Vec3,
    ) -> Option<QueryRayResult> {
        let trf = self.physics.transform(physics);

        let start = translation_world_to_local(start, &trf) + Vec3::ONE / 2.0;
        let direction = direction_world_to_local(direction, &trf);

        let ray = mechaia::voxel::common::query::Ray::new(start, direction);
        let mut free = None;
        for pos in ray.take(1024) {
            if self.voxels.get(pos).is_some() {
                return Some(QueryRayResult {
                    occupied: LocalWorld::from_local(pos, &trf),
                    free: free.map(|local| LocalWorld::from_local(local, &trf)),
                });
            }
            free = Some(pos);
        }
        None
    }

    pub fn step(&mut self, physics: &mut Physics) {
        self.physics.step(physics)
    }

    pub fn render(&self, physics: &Physics, collector: &mut DrawCollector) {
        let trf = self.physics.transform(physics);

        for (pos, blk) in self.voxels.iter() {
            let mut trfs = Vec::new();
            let mut push = |tr, rot|
                trfs.push(trf.apply_to_transform(&physics3d::Transform {
                    translation: tr,
                    rotation: rot,
                }).into());
            push(pos.as_vec3(), blk.orientation());
            // FIXME
            if blk.id == 4 {
                let w = *self.physics.pos_to_wheel.get(&pos).unwrap();
                let (axle, tire) = self.physics.vehicle_body.wheel_local_transform(w);
                push(axle.translation, axle.rotation);
                push(tire.translation, tire.rotation);
            }
            collector.solid.push(blk.id.into(), 0, &trfs)
        }
    }

    pub fn transform(&self, physics: &Physics) -> physics3d::Transform {
        self.physics.transform(physics)
    }

    pub fn set_input_controls(&mut self, controls: &InputControls) {
        let wheel_max_torque = 100.0;
        let wheel_max_angle = core::f32::consts::FRAC_PI_6;
        let forward = controls.forward.clamp(-1.0, 1.0);
        let brake = f32::from(forward == 0.0);
        let pan = controls.pan.clamp(-1.0, 1.0);
        for (&pos, &w) in self.physics.pos_to_wheel.iter() {
            //let forward = if pos.x < 0 { -forward } else { forward };
            let forward = if pos.y < 0 { -forward } else { forward };
            let pan = if pos.x < 0 { 0.0 } else { pan };
            self.physics.vehicle_body.set_wheel_angle(w, pan * wheel_max_angle);
            self.physics.vehicle_body.set_wheel_torque(w, forward * wheel_max_torque);
            self.physics.vehicle_body.set_wheel_brake(w, brake);
        }
    }
}

pub struct QueryRayResult {
    pub occupied: LocalWorld,
    /// May be none if the ray started inside an object.
    pub free: Option<LocalWorld>,
}

pub struct LocalWorld {
    pub local: IVec3,
    pub world: Vec3,
}

impl LocalWorld {
    fn from_local(local: IVec3, local_to_world: &physics3d::Transform) -> Self {
        let trf = local_to_world;
        Self {
            local,
            world: (trf.rotation * local.as_vec3()) + trf.translation,
        }
    }
}

fn translation_world_to_local(world: Vec3, local_to_world: &physics3d::Transform) -> Vec3 {
    local_to_world.rotation.inverse() * (world - local_to_world.translation)
}

fn direction_world_to_local(world: Vec3, local_to_world: &physics3d::Transform) -> Vec3 {
    //local_to_world.rotation.inverse() * world
    local_to_world.rotation.inverse() * world
}

#[derive(Clone)]
pub struct Block {
    pub id: u8,
    orientation: u8,
}

impl Block {
    pub fn new(id: u8, direction: u8, rotation: u8) -> Self {
        let mut s = Self { id, orientation: 0 };
        s.set_direction(direction);
        s.set_rotation(rotation);
        s
    }

    pub fn direction(&self) -> u8 {
        self.orientation & 7
    }

    pub fn rotation(&self) -> u8 {
        self.orientation >> 3
    }

    pub fn set_direction(&mut self, direction: u8) {
        debug_assert!(direction < 6);
        self.orientation &= !7;
        self.orientation |= direction;
    }

    pub fn set_rotation(&mut self, rotation: u8) {
        debug_assert!(rotation < 4);
        self.orientation &= 7;
        self.orientation |= rotation << 3;
    }

    pub fn orientation(&self) -> Quat {
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
