use super::Block;
use crate::Physics;
use mechaia::{
    math::{IVec3, Vec3},
    physics3d::{wheel::{VehicleBody, Wheel, WheelHandle, WheelSuspension}, ColliderHandle, ColliderProperties, RigidBodyHandle, Transform},
};
use std::collections::HashMap;

pub struct Body {
    body: RigidBodyHandle,
    colliders: HashMap<IVec3, ColliderHandle>,
    pub(super) pos_to_wheel: HashMap<IVec3, WheelHandle>,
    pub(super) vehicle_body: VehicleBody,
}

impl Body {
    pub fn new(physics: &mut Physics) -> Self {
        Self {
            body: physics.engine.make_dynamic_rigid_body(),
            colliders: Default::default(),
            pos_to_wheel: Default::default(),
            vehicle_body: VehicleBody::new(),
        }
    }

    pub fn add(&mut self, physics: &mut Physics, pos: IVec3, block: &Block) {
        let h = physics.engine.add_collider(
            self.body,
            physics.shape_cache.unit_cube,
            &ColliderProperties {
                local_transform: Transform {
                    rotation: block.orientation(),
                    translation: pos.as_vec3(),
                },
                // again, my ass
                friction: 0.8,
                bounciness: 0.1,
                user_data: 0,
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
                //static_friction: 0.8,
                static_friction: 1.0,
                dynamic_friction: 0.6,
                suspension: WheelSuspension {
                    max_length: 0.5,
                    force_per_distance: 600.0,
                    damp_per_velocity: 50.0,
                },
            };
            let w = self.vehicle_body.add_wheel(w);
            self.pos_to_wheel.insert(pos, w);
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
        }
    }

    pub fn step(&mut self, physics: &mut Physics) {
        self.vehicle_body.apply(&mut physics.engine, self.body);
    }

    pub fn transform(&self, physics: &Physics) -> Transform {
        physics.engine.rigid_body_transform(self.body)
    }
}
