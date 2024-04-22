mod physics;
mod turret;

use crate::{gfx::DrawCollector, physics::Physics};
use mechaia::{
    math::{IVec3, Quat, Vec3, Vec3Swizzles},
    model::Collection,
    util::Transform,
    voxel,
};
use std::collections::HashMap;

pub struct Vehicle {
    physics: physics::Body,
    voxels: voxel::common::Map<Block>,
    turrets: HashMap<IVec3, Turret>,
}

struct Turret {
    pan: f32,
    tilt: f32,
}

#[derive(Default)]
pub struct InputControls {
    pub forward: f32,
    pub pan: f32,
    pub tilt: f32,
    pub target: Vec3,
    pub fire: bool,
}

pub struct BlockSet {
    blocks: Vec<BlockSetEntry>,
    name_to_id: HashMap<Box<str>, u32>,
}

struct BlockSetEntry {
    name: Box<str>,
    mesh: u32,
    armature: u32,
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
                        armature: if model.armature_index == usize::MAX {
                            u32::MAX
                        } else {
                            model.armature_index.try_into().unwrap()
                        },
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

    pub fn get_armature(&self, id: u32) -> u32 {
        self.blocks[usize::try_from(id).unwrap()].armature
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
            turrets: Default::default(),
        }
    }

    /// Add a block without any sanity checks.
    pub fn force_add(&mut self, physics: &mut Physics, pos: IVec3, block: Block) {
        self.physics.add(physics, pos, &block);
        if block.id == 5 {
            self.turrets.insert(
                pos,
                Turret {
                    pan: 0.0,
                    tilt: 0.0,
                },
            );
        }
        self.voxels.insert(pos, block);
    }

    /// Remove a block without any sanity checks.
    pub fn force_remove(&mut self, physics: &mut Physics, pos: IVec3) {
        self.physics.remove(physics, pos);
        self.voxels.remove(pos);
        self.turrets.remove(&pos);
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
                    occupied: pos,
                    free,
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
        collection: &Collection,
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
            let mut push = |tr, rot| trfs.push(trf.apply_to_transform(&Transform::new(tr, rot)).into());
            if blk.id != 5 {
                push(pos.as_vec3(), blk.orientation());
            }
            // FIXME
            if blk.id == 4 {
                let w = *self.physics.pos_to_wheel.get(&pos).unwrap();
                let (start_axle, start_tire) = self.physics.prev_wheel_transforms.get(&w).unwrap();
                let (end_axle, end_tire) = self.physics.vehicle_body.wheel_local_transform(w);
                let axle = start_axle.interpolate(&end_axle, interpolation);
                let tire = start_tire.interpolate(&end_tire, interpolation);
                push(axle.translation, axle.rotation);
                push(tire.translation, tire.rotation);
            } else if blk.id == 5 {
                let turret = self.turrets.get(&pos).unwrap();
                let t = turret::turret_model_to_world_transform(
                    &trf,
                    pos,
                    blk,
                    block_set,
                    collection,
                    turret.pan,
                    turret.tilt,
                    true,
                );
                t.map(|t| trfs.push(t.into()));
            }
            let mesh = block_set.get_mesh(blk.id);
            collector.solid.push(mesh, 0, &trfs)
        }
    }

    pub fn transform(&self, physics: &Physics) -> Transform {
        self.physics.transform(physics)
    }

    pub fn set_transform(&mut self, physics: &mut Physics, transform: &Transform) {
        self.physics.set_transform(physics, transform)
    }

    pub fn set_input_controls(
        &mut self,
        block_set: &BlockSet,
        projectile_model: u32,
        collection: &Collection,
        physics: &Physics,
        controls: &InputControls,
    ) -> Vec<(Transform, f32)> {
        // movement
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

        // aim target
        let trf = self.physics.transform(physics);
        let target = trf.apply_to_translation_inv(controls.target);

        for (&pos, turret) in self.turrets.iter_mut() {
            let blk = self.voxels.get(pos).unwrap();
            let trf = Transform::new(pos.as_vec3(), blk.orientation());
            let target = trf.apply_to_translation_inv(target);

            let armature = block_set.get_armature(blk.id);
            let armature = &collection.armatures[armature as usize];
            let trfs = armature.apply(&Transform::IDENTITY, &[Transform::IDENTITY; 3], false);
            let tilt_joint_offset = trfs[2].translation.xz();

            (turret.pan, turret.tilt) = turret::calc_pan_tilt(target, tilt_joint_offset);
        }

        // fire
        let mut projectiles = Vec::new();
        if controls.fire {
            let model = &collection.models[projectile_model as usize];
            let mesh = model.mesh_index;
            let armature = &collection.armatures[model.armature_index];
            for (&pos, turret) in self.turrets.iter_mut() {
                let blk = self.voxels.get(pos).unwrap();
                let t = turret::turret_model_to_world_transform(
                    &trf,
                    pos,
                    blk,
                    block_set,
                    collection,
                    turret.pan,
                    turret.tilt,
                    false,
                );
                let muzzle_trf = t[2];
                let dir = muzzle_trf.apply_to_direction(Vec3::Y);
                let ray = physics
                    .engine
                    .cast_ray(muzzle_trf.translation, dir * 1e3, None);
                let dist = ray.map_or(1e3, |r| r.distance);
                projectiles.push((muzzle_trf, dist));
            }
        }

        projectiles
    }
}

pub struct QueryRayResult {
    pub occupied: IVec3,
    /// May be none if the ray started inside an object.
    pub free: Option<IVec3>,
}

fn translation_world_to_local(world: Vec3, local_to_world: &Transform) -> Vec3 {
    local_to_world.rotation.inverse() * (world - local_to_world.translation)
}

fn direction_world_to_local(world: Vec3, local_to_world: &Transform) -> Vec3 {
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

    pub fn load_v0_text(&mut self, block_set: &BlockSet, physics: &mut Physics, text: &str) {
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
