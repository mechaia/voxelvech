use mechaia::{
    math::Vec3,
    model::Collection,
    physics3d::{self, ShapeHandle},
};

// set it extremely high so we remember to implement interpolation
//const TIME_DELTA: f32 = 1.0 / 10.0;
const TIME_DELTA: f32 = 1.0 / 30.0;
//const TIME_DELTA: f32 = 1.0 / 100.0;

pub struct Physics {
    pub engine: physics3d::Physics,
    pub shape_cache: ShapeCache,
}

pub struct ShapeCache {
    pub unit_cube: ShapeHandle,
}

impl Physics {
    pub fn new() -> Self {
        let mut engine = physics3d::Physics::new();
        engine.set_time_delta(TIME_DELTA);
        let shape_cache = ShapeCache::new(&mut engine);
        Self {
            engine,
            shape_cache,
        }
    }
}

impl ShapeCache {
    fn new(engine: &mut physics3d::Physics) -> Self {
        Self {
            unit_cube: engine.make_cuboid_shape(Vec3::ONE / 2.0),
        }
    }
}
