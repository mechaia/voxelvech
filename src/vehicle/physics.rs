use {
    super::{sound::WheelSound, Block, BlockSet, BlockSetEntryData, VehicleSound},
    crate::Physics,
    mechaia::{
        math::{IVec3, Vec3},
        physics3d::{
            wheel::{VehicleBody, Wheel, WheelHandle, WheelSuspension},
            ColliderHandle, ColliderProperties, RigidBodyHandle, SetRigidBodyProperties,
        },
        util::{
            math::fixed::{U0d32, U32d32},
            Transform,
        },
    },
    std::collections::HashMap,
};

pub(super) struct Body {
    body: RigidBodyHandle,
    colliders: HashMap<IVec3, ColliderHandle>,
    pub(super) pos_to_wheel: HashMap<IVec3, WheelInfo>,
    pub(super) vehicle_body: VehicleBody,

    pub(super) prev_transform: Transform,

    pub(super) user_data: u32,
}

pub(super) struct WheelInfo {
    pub handle: WheelHandle,
    pub prev_axle: Transform,
    pub prev_tire: Transform,
}

impl Body {
    pub fn new(physics: &mut Physics, user_data: u32) -> Self {
        Self {
            body: physics.engine.make_dynamic_rigid_body(),
            colliders: Default::default(),
            pos_to_wheel: Default::default(),
            vehicle_body: VehicleBody::new(),

            prev_transform: Transform::IDENTITY,

            user_data,
        }
    }

    pub fn add(
        &mut self,
        physics: &mut Physics,
        sound: &mut VehicleSound,
        block_set: &BlockSet,
        pos: IVec3,
        block: &Block,
    ) {
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
        if let BlockSetEntryData::Wheel { .. } = &block_set[block.id].data {
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
                    damp_per_velocity: 500.0,
                },
            };
            let handle = self.vehicle_body.add_wheel(w);
            let (prev_axle, prev_tire) = self.vehicle_body.wheel_local_transform(handle);
            self.pos_to_wheel.insert(
                pos,
                WheelInfo {
                    handle,
                    prev_axle,
                    prev_tire,
                },
            );
            sound.wheels.insert(pos, WheelSound::default());
        }
    }

    pub fn remove(&mut self, physics: &mut Physics, sound: &mut VehicleSound, pos: IVec3) {
        let handle = self
            .colliders
            .remove(&pos)
            .expect("no collider at location");
        physics.engine.remove_collider(self.body, handle);

        if let Some(w) = self.pos_to_wheel.remove(&pos) {
            self.vehicle_body.remove_wheel(w.handle);
        }
    }

    pub fn step(&mut self, physics: &mut Physics) {
        self.prev_transform = self.transform(physics);
        for info in self.pos_to_wheel.values_mut() {
            (info.prev_axle, info.prev_tire) = self.vehicle_body.wheel_local_transform(info.handle);
        }
        self.vehicle_body.apply(&mut physics.engine, self.body);
    }

    pub fn apply_sounds(&mut self, sound: &mut VehicleSound) {
        for (pos, wheel) in self.pos_to_wheel.iter_mut() {
            let snd = sound.wheels.get_mut(pos).unwrap();
            snd.speed =
                U32d32::from_f32(self.vehicle_body.wheel_angular_velocity(wheel.handle).abs());
        }
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
            self.vehicle_body.remove_wheel(w.handle);
        }
    }

    pub fn destroy(mut self, physics: &mut Physics) {
        self.clear(physics);
        physics.engine.destroy_rigid_body(self.body);
    }
}
