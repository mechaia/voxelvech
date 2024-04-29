use crate::{
    gfx::{Draw, Gfx, MeshSetHandle},
    physics::Physics,
    State,
};
use mechaia::{
    math::Vec3,
    model::Collection,
    physics3d::{ColliderProperties, RigidBodyHandle},
    render::resource::camera::CameraView,
    util::Transform,
};

pub trait Scenario {
    fn step(&mut self, state: &mut State);
    fn camera(&self, aspect: f32) -> CameraView;
    fn draw(&mut self, state: &mut State, draw: &mut Draw<'_>);
    fn destroy(self: Box<Self>, state: &mut State, gfx: &mut Gfx);
}

pub struct World {
    body: RigidBodyHandle,
    meshes: Vec<(Transform, u32)>,
    mesh_set: MeshSetHandle,
    collection: Collection,
}

impl World {
    pub fn load_from_asset(path: &str, physics: &mut Physics, gfx: &mut Gfx) -> Option<Self> {
        let collection = crate::data::read_asset_gltf(path)?;

        let body = physics.engine.make_fixed_rigid_body();
        let mut meshes = Vec::new();

        // load map
        let root = &collection.scenes[0];
        assert!(root
            .properties()
            .name
            .as_ref()
            .is_some_and(|s| s.starts_with("map.")));
        add_objects(
            physics,
            body,
            &collection,
            &Transform::IDENTITY,
            root,
            &mut meshes,
        );

        let mesh_set = gfx.add_mesh_set(&collection);

        Some(Self {
            body,
            meshes,
            mesh_set,
            collection,
        })
    }

    pub fn draw(&self, draw: &mut Draw) {
        for &(trf, mesh) in self.meshes.iter() {
            draw.collector
                .solid
                .push(self.mesh_set, mesh, 2, &[trf.into()]);
        }
    }

    pub fn destroy(self, physics: &mut Physics, gfx: &mut Gfx) {}
}

fn add_objects(
    physics: &mut Physics,
    body: RigidBodyHandle,
    collection: &Collection,
    transform: &Transform,
    node: &mechaia::model::Node,
    meshes: &mut Vec<(Transform, u32)>,
) {
    if let mechaia::model::Node::Parent { children, .. } = node {
        for (trf, c) in children.iter() {
            let trf = transform.apply_to_transform(trf);
            add_objects(physics, body, collection, &trf, c, meshes);
        }
    }

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
            "halfspace" => physics.engine.make_halfspace_shape(Vec3::Z),
            "convex" => {
                let mesh = &collection.meshes[model.mesh_index];
                physics
                    .engine
                    .make_convex_hull_shape(mesh.vertices.as_slices().0)
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
        physics.engine.add_collider(body, shape, &properties);
        meshes.push((*transform, model.mesh_index.try_into().unwrap()));
    }
}
