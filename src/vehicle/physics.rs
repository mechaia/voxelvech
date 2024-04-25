use super::Block;
use crate::Physics;
use mechaia::{
    math::{IVec3, Vec3},
    physics3d::{
        wheel::{VehicleBody, Wheel, WheelHandle, WheelSuspension},
        ColliderHandle, ColliderProperties, RigidBodyHandle, SetRigidBodyProperties,
    },
    util::Transform,
};
use std::collections::HashMap;

pub(super) struct Body {
    body: RigidBodyHandle,
    colliders: HashMap<IVec3, ColliderHandle>,
    pub(super) pos_to_wheel: HashMap<IVec3, WheelHandle>,
    pub(super) vehicle_body: VehicleBody,

    pub(super) prev_transform: Transform,
    pub(super) prev_wheel_transforms: HashMap<WheelHandle, (Transform, Transform)>,

    pub(super) user_data: u32,
}

impl Body {
    pub fn new(physics: &mut Physics, user_data: u32) -> Self {
        Self {
            body: physics.engine.make_dynamic_rigid_body(),
            colliders: Default::default(),
            pos_to_wheel: Default::default(),
            vehicle_body: VehicleBody::new(),

            prev_transform: Transform::IDENTITY,
            prev_wheel_transforms: Default::default(),

            user_data,
        }
    }

    pub fn add(&mut self, physics: &mut Physics, pos: IVec3, block: &Block) {
        let h = physics.engine.add_collider(
            self.body,
            physics.shape_cache.unit_cube,
            &ColliderProperties {
                local_transform: Transform::new(pos.as_vec3(), block.orientation()),
                // again, my ass
                friction: 0.8,
                bounciness: 0.1,
                user_data: self.user_data,
            },
        );
        self.colliders.insert(pos, h);

        // FIXME id yuck
        if block.id == 4 {
            let w = Wheel {
                transform: Transform {
                    translation: pos.as_vec3(),
                    rotation: block.orientation(),
                },
                radius: 1.5,
                static_friction: 0.8,
                dynamic_friction: 0.6,
                suspension: WheelSuspension {
                    max_length: 0.6,
                    force_per_distance: 5000.0,
                    damp_per_velocity: 50.0,
                },
            };
            let w = self.vehicle_body.add_wheel(w);
            self.pos_to_wheel.insert(pos, w);
            let trfs = self.vehicle_body.wheel_local_transform(w);
            self.prev_wheel_transforms.insert(w, trfs);
        }
    }

    pub fn remove(&mut self, physics: &mut Physics, pos: IVec3) {
        let handle = self
            .colliders
            .remove(&pos)
            .expect("no collider at location");
        physics.engine.remove_collider(self.body, handle);

        if let Some(w) = self.pos_to_wheel.remove(&pos) {
            self.vehicle_body.remove_wheel(w);
            self.prev_wheel_transforms.remove(&w);
        }
    }

    pub fn step(&mut self, physics: &mut Physics) {
        self.prev_transform = self.transform(physics);
        for (&w, trfs) in self.prev_wheel_transforms.iter_mut() {
            *trfs = self.vehicle_body.wheel_local_transform(w);
        }
        self.vehicle_body.apply(&mut physics.engine, self.body);
    }

    pub fn transform(&self, physics: &Physics) -> Transform {
        physics.engine.rigid_body_transform(self.body)
    }

    pub fn set_transform(&mut self, physics: &mut Physics, transform: &Transform) {
        physics.engine.set_rigid_body_properties(
            self.body,
            &SetRigidBodyProperties {
                world_transform: Some(*transform),
                linear_velocity: Some(Vec3::ZERO),
                angular_velocity: Some(Vec3::ZERO),
                enable_ccd: None,
            },
        );
    }

    pub fn center_of_mass(&self, physics: &Physics) -> Vec3 {
        physics.engine.rigid_body_center_of_mass(self.body)
    }

    pub fn clear(&mut self, physics: &mut Physics) {
        for (_, c) in self.colliders.drain() {
            physics.engine.remove_collider(self.body, c)
        }
        for (_, w) in self.pos_to_wheel.drain() {
            self.vehicle_body.remove_wheel(w);
            self.prev_wheel_transforms.remove(&w);
        }
    }

    pub fn destroy(mut self, physics: &mut Physics) {
        self.clear(physics);
        physics.engine.destroy_rigid_body(self.body);
    }
}
