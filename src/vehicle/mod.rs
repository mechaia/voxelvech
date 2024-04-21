mod physics;

use std::collections::HashMap;

use crate::{gfx::DrawCollector, physics::Physics};
use mechaia::{
    math::{IVec3, Quat, Vec3},
    model::Collection,
    physics3d, voxel,
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

pub struct BlockSet {
    blocks: Vec<BlockSetEntry>,
    name_to_id: HashMap<Box<str>, u32>,
}

struct BlockSetEntry {
    name: Box<str>,
    mesh: u32,
}

impl BlockSet {
    pub fn from_collection(collection: &Collection) -> Self {
        let mut blocks = Vec::new();
        let mut name_to_id = HashMap::new();

        // FIXME we need a better way to manage these damn assets
        for root in collection.scenes.iter() {
            for node in root.descendants() {
                if let Some(name) = node
                    .properties()
                    .name
                    .as_ref()
                    .filter(|n| n.starts_with("block."))
                {
                    let mechaia::model::Node::Leaf { model, .. } = node else {
                        panic!("todo: handle multi-mesh blocks (or not?)");
                    };
                    let model = &collection.models[*model];
                    let name = Box::<str>::from(&name[6..]);
                    let i = blocks.len().try_into().unwrap();
                    blocks.push(BlockSetEntry {
                        name: name.clone(),
                        mesh: model.mesh_index.try_into().unwrap(),
                    });
                    name_to_id.insert(name, i);
                }
            }
        }

        Self { blocks, name_to_id }
    }

    pub fn cube_id(&self) -> u32 {
        self.get_id("cube").expect("no cube defined")
    }

    pub fn get_id(&self, name: &str) -> Option<u32> {
        self.name_to_id.get(name).copied()
    }

    pub fn get_name(&self, id: u32) -> &str {
        &*self.blocks[usize::try_from(id).unwrap()].name
    }

    pub fn get_mesh(&self, id: u32) -> u32 {
        self.blocks[usize::try_from(id).unwrap()].mesh
    }

    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    pub fn len_u32(&self) -> u32 {
        self.len().try_into().unwrap()
    }
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
        self.physics.step(physics);
    }

    pub fn render(
        &self,
        block_set: &BlockSet,
        physics: &Physics,
        interpolation: f32,
        collector: &mut DrawCollector,
    ) {
        let end_trf = self.physics.transform(physics);
        let trf = self
            .physics
            .prev_transform
            .interpolate(&end_trf, interpolation);

        for (pos, blk) in self.voxels.iter() {
            let mut trfs = Vec::new();
            let mut push = |tr, rot| {
                trfs.push(
                    trf.apply_to_transform(&physics3d::Transform {
                        translation: tr,
                        rotation: rot,
                    })
                    .into(),
                )
            };
            push(pos.as_vec3(), blk.orientation());
            // FIXME
            if blk.id == 4 {
                let w = *self.physics.pos_to_wheel.get(&pos).unwrap();
                let (start_axle, start_tire) = self.physics.prev_wheel_transforms.get(&w).unwrap();
                let (end_axle, end_tire) = self.physics.vehicle_body.wheel_local_transform(w);
                let axle = start_axle.interpolate(&end_axle, interpolation);
                let tire = start_tire.interpolate(&end_tire, interpolation);
                push(axle.translation, axle.rotation);
                push(tire.translation, tire.rotation);
            }
            let mesh = block_set.get_mesh(blk.id);
            collector.solid.push(mesh, 0, &trfs)
        }
    }

    pub fn transform(&self, physics: &Physics) -> physics3d::Transform {
        self.physics.transform(physics)
    }

    pub fn set_transform(&mut self, physics: &mut Physics, transform: &physics3d::Transform) {
        self.physics.set_transform(physics, transform)
    }

    pub fn set_input_controls(&mut self, physics: &Physics, controls: &InputControls) {
        let com = self.physics.center_of_mass(physics);

        let wheel_max_torque = 1000.0;
        let wheel_max_angle = core::f32::consts::FRAC_PI_6;
        let forward = controls.forward.clamp(-1.0, 1.0);
        let brake = f32::from(forward == 0.0);
        let pan = controls.pan.clamp(-1.0, 1.0);

        for (&pos, &w) in self.physics.pos_to_wheel.iter() {
            //let forward = if pos.x < 0 { -forward } else { forward };
            let forward = if pos.y < 0 { -forward } else { forward };
            let pan = if (pos.x as f32) < com.x { -pan } else { pan };
            self.physics
                .vehicle_body
                .set_wheel_angle(w, pan * wheel_max_angle);
            self.physics
                .vehicle_body
                .set_wheel_torque(w, forward * wheel_max_torque);
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
    pub id: u32,
    orientation: u8,
}

impl Block {
    pub fn new(id: u32, direction: u8, rotation: u8) -> Self {
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

/// (De)serializer for v0 text format
/// It is extremely inefficient and garbage but works well enough
impl Vehicle {
    pub fn save_v0_text<R>(
        &self,
        block_set: &BlockSet,
        out: &mut dyn FnMut(&[u8]) -> Result<(), R>,
    ) -> Result<(), R> {
        // collect blocks and sort to ensure minimal diffs
        let mut v = self.voxels.iter().collect::<Vec<_>>();
        v.sort_unstable_by_key(|(v, _)| (v.x, v.y, v.z));
        for (k, b) in v {
            out(k.x.to_string().as_bytes())?;
            out(b" ")?;
            out(k.y.to_string().as_bytes())?;
            out(b" ")?;
            out(k.z.to_string().as_bytes())?;
            out(b" ")?;
            let name = block_set.get_name(b.id);
            out(name.as_bytes())?;
            out(b" ")?;
            out(b.orientation.to_string().as_bytes())?;
            out(b"\n")?;
        }
        Ok(())
    }

    pub fn load_v0_text(
        &mut self,
        block_set: &BlockSet,
        physics: &mut Physics,
        text: &str,
    ) {
        self.voxels.clear();
        self.physics.clear(physics);

        for (i, line) in text.lines().enumerate() {
            let line = line.split_once('#').map_or(line, |l| l.0);
            let mut it = line.split_whitespace();
            let mut f = || it.next().map(|v| v.parse::<i32>().expect("coordinate"));
            let Some(x) = f() else { continue };
            let y = f().expect("y");
            let z = f().expect("z");
            let name = it.next().expect("name");
            let orientation = it
                .next()
                .expect("orientation")
                .parse::<u8>()
                .expect("orientation");
            assert!(it.next().is_none(), "garbage at end of line {i}");

            let blk = Block {
                id: block_set.get_id(name).unwrap_or_else(|| { 
                    crate::log::error(format!("block {name} not recognized, replacing with cube"));
                    block_set.cube_id()
                }),
                orientation,
            };
            self.force_add(physics, IVec3::new(x, y, z), blk);
        }
    }
}
