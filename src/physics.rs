use mechaia::{
    math::Vec3,
    model::Collection,
    physics3d::{self, ColliderProperties, RigidBodyHandle, ShapeHandle},
    util::Transform,
};

use crate::gfx::DrawCollector;

// set it extremely high so we remember to implement interpolation
//const TIME_DELTA: f32 = 1.0 / 10.0;
const TIME_DELTA: f32 = 1.0 / 30.0;
//const TIME_DELTA: f32 = 1.0 / 100.0;

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
    meshes: Vec<(Transform, u32)>,
}

impl Physics {
    pub fn new(collection: &Collection) -> Self {
        let mut engine = physics3d::Physics::new();
        engine.set_time_delta(TIME_DELTA);
        let shape_cache = ShapeCache::new(&mut engine);
        let world = World::new(&mut engine, collection);
        Self {
            engine,
            shape_cache,
            world,
        }
    }

    pub fn render(&self, collector: &mut DrawCollector) {
        let gtrf = self.engine.rigid_body_transform(self.world.body);
        for (trf, mesh) in self.world.meshes.iter() {
            let trf = gtrf.apply_to_transform(trf);
            collector.solid.push(*mesh, 2, &[trf.into()]);
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

impl World {
    fn new(engine: &mut physics3d::Physics, collection: &Collection) -> Self {
        let body = engine.make_fixed_rigid_body();

        let mut meshes = Vec::new();

        // load map
        let mechaia::model::Node::Parent {
            children: scenes, ..
        } = &collection.scenes[0]
        else {
            todo!()
        };
        for (trf, root) in scenes.iter() {
            if root
                .properties()
                .name
                .as_ref()
                .is_some_and(|s| s.starts_with("map."))
            {
                add_objects(engine, body, collection, &trf, root, &mut meshes);
                break;
            }
        }
        Self { body, meshes }
    }
}

fn add_objects(
    physics: &mut physics3d::Physics,
    body: RigidBodyHandle,
    collection: &Collection,
    transform: &Transform,
    node: &mechaia::model::Node,
    meshes: &mut Vec<(Transform, u32)>,
) {
    'add: {
        let prop = node.properties();
        let Some(name) = prop.name.as_deref() else {
            break 'add;
        };
        let Some((ty, _)) = name.split_once('.') else {
            break 'add;
        };
        let mechaia::model::Node::Leaf { model, .. } = node else {
            break 'add;
        };
        let model = &collection.models[*model];
        let shape = match ty {
            "halfspace" => physics.make_halfspace_shape(Vec3::Z),
            "convex" => {
                //physics.make_convex_mesh_indexed_shape(mesh.vertices.as_slices().0, &mesh.indices);
                let mesh = &collection.meshes[model.mesh_index];
                physics.make_convex_hull_shape(mesh.vertices.as_slices().0)
            }
            "map" => break 'add,
            _ => todo!("{ty}"),
        };
        let properties = &ColliderProperties {
            local_transform: *transform,
            // numbers pulled from my ass
            friction: 0.8,
            bounciness: 0.05,
            user_data: 0,
        };
        physics.add_collider(body, shape, &properties);
        meshes.push((*transform, model.mesh_index.try_into().unwrap()));
    }

    let mechaia::model::Node::Parent { children, .. } = node else {
        return;
    };
    for (trf, c) in children.iter() {
        let trf = transform.apply_to_transform(trf);
        add_objects(physics, body, collection, &trf, c, meshes);
    }
}
