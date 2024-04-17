use mechaia::{
    math::{Quat, Vec3},
    physics3d::{
        self, ColliderHandle, ColliderProperties, RigidBodyHandle, ShapeHandle, Transform,
    },
};

use crate::gfx::DrawCollector;

// set it extremely high so we remember to implement interpolation
//const TIME_DELTA: f32 = 1.0 / 10.0;
const TIME_DELTA: f32 = 1.0 / 100.0;

pub struct Physics {
    pub engine: physics3d::Physics,
    pub shape_cache: ShapeCache,
    world: World,
}

pub struct ShapeCache {
    pub unit_cube: ShapeHandle,
}

struct World {
    body: RigidBodyHandle,
    shapes: Vec<ShapeHandle>,
    colliders: Vec<ColliderHandle>,
}

impl Physics {
    pub fn new() -> Self {
        let mut engine = physics3d::Physics::new();
        engine.set_time_delta(TIME_DELTA);
        let shape_cache = ShapeCache::new(&mut engine);
        let world = World::new(&mut engine);
        Self {
            engine,
            shape_cache,
            world,
        }
    }

    pub fn render(&self, collector: &mut DrawCollector) {
        let mut trf = self.engine.rigid_body_transform(self.world.body);
        trf.translation += Vec3::new(0.0, 0.0, -10.0);
        collector.solid.push(5, 0, &[trf.into()]);
    }
}

impl ShapeCache {
    fn new(engine: &mut physics3d::Physics) -> Self {
        Self {
            unit_cube: engine.make_cuboid_shape(Vec3::ONE / 2.0),
        }
    }
}

impl World {
    fn new(engine: &mut physics3d::Physics) -> Self {
        let body = engine.make_fixed_rigid_body();
        let mut shapes = Vec::new();
        let mut colliders = Vec::new();
        // add world floor
        {
            let floor_shape = engine.make_halfspace_shape(Vec3::Z);
            let properties = &ColliderProperties {
                local_transform: Transform {
                    rotation: Quat::IDENTITY,
                    //translation: Vec3::new(0.0, 0.0, -256.0),
                    translation: Vec3::new(0.0, 0.0, -10.0),
                },
                // numbers pulled from my ass
                friction: 0.8,
                bounciness: 0.05,
                user_data: 0,
            };
            let floor_collider = engine.add_collider(body, floor_shape, &properties);
            shapes.push(floor_shape);
            colliders.push(floor_collider);
        }
        Self {
            body,
            shapes,
            colliders,
        }
    }
}
