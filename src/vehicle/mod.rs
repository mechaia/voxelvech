mod block;
mod block_set;
mod damage;
mod physics;
mod sound;
mod turret;

use crate::{
    gfx::{Draw, DrawCollector},
    physics::Physics,
};
use mechaia::{
    math::{IVec3, Vec3, Vec3Swizzles},
    model::Collection,
    util::Transform,
    voxel,
};
use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

pub use {
    block::Block,
    block_set::{BlockSet, BlockSetEntry, BlockSetEntryData},
    damage::DamageAccumulator,
    sound::VehicleSound,
};

pub struct Vehicle {
    physics: physics::Body,
    voxels: voxel::common::Map<Block>,
    damage: damage::Body,
    turrets: HashMap<IVec3, Turret>,
}

struct Turret {
    pan: f32,
    tilt: f32,
    next_fire: Instant,
}

pub struct Projectile {
    pub start: Transform,
    pub length: f32,
    pub mesh_index: u32,
    pub armature_index: u32,
}

#[derive(Default)]
pub struct InputControls {
    pub forward: f32,
    pub pan: f32,
    pub tilt: f32,
    pub target: Vec3,
    pub fire: bool,
}

impl Vehicle {
    pub fn new(physics: &mut Physics, user_data: u32) -> Self {
        Self {
            physics: physics::Body::new(physics, user_data),
            voxels: voxel::common::Map::new(),
            turrets: Default::default(),
            damage: Default::default(),
        }
    }

    /// Add a block without any sanity checks.
    pub fn force_add(
        &mut self,
        physics: &mut Physics,
        sound: &mut VehicleSound,
        block_set: &BlockSet,
        pos: IVec3,
        block: Block,
    ) {
        if damage::Body::is_core(block_set, block.id) && self.damage.has_core() {
            crate::log::warn("vehicle already has a core");
            return;
        }

        self.physics.add(physics, sound, block_set, pos, &block);
        if let BlockSetEntryData::Weapon { .. } = &block_set[block.id].data {
            self.turrets.insert(
                pos,
                Turret {
                    pan: 0.0,
                    tilt: 0.0,
                    next_fire: Instant::now(),
                },
            );
        }
        self.voxels.insert(pos, block);
        self.damage.insert(block_set, pos, block);
    }

    /// Remove a block without any sanity checks.
    pub fn force_remove(&mut self, physics: &mut Physics, sound: &mut VehicleSound, pos: IVec3) {
        self.physics.remove(physics, sound, pos);
        self.voxels.remove(pos);
        self.turrets.remove(&pos);
        self.damage.remove(pos);
    }

    pub fn query_ray_params(
        &self,
        physics: &Physics,
        start: Vec3,
        direction: Vec3,
    ) -> (Vec3, Vec3) {
        let trf = self.physics.transform(physics);
        let start = translation_world_to_local(start, &trf) + Vec3::ONE / 2.0;
        let direction = direction_world_to_local(direction, &trf);
        (start, direction)
    }

    pub fn query_ray(
        &mut self,
        physics: &Physics,
        start: Vec3,
        direction: Vec3,
        ignore_dead: bool,
    ) -> Option<QueryRayResult> {
        let (start, direction) = self.query_ray_params(physics, start, direction);

        let ray = mechaia::voxel::common::query::Ray::new(start, direction);
        let mut free = None;
        for pos in ray.take(1024) {
            if self.voxels.get(pos).is_some() {
                if !ignore_dead || self.damage.health(pos) > 0 {
                    return Some(QueryRayResult {
                        occupied: pos,
                        free,
                    });
                }
            }
            free = Some(pos);
        }
        None
    }

    pub fn step(&mut self, physics: &mut Physics) {
        self.physics.step(physics);
    }

    pub fn apply_sounds(&mut self, sound: &mut VehicleSound) {
        self.physics.apply_sounds(sound)
    }

    pub fn render(
        &self,
        state: &crate::State,
        physics: &Physics,
        interpolation: f32,
        draw: &mut Draw<'_>,
    ) {
        let end_trf = self.physics.transform(physics);
        let trf = self
            .physics
            .prev_transform
            .interpolate(&end_trf, interpolation);

        for (pos, blk) in self.voxels.iter() {
            let hp = self.damage.health(pos);
            if hp == 0 {
                continue;
            }

            let mut trfs = Vec::new();
            let mut push = |t| trfs.push(trf.apply_to_transform(&t).into());

            let mesh = match &state.block_set[blk.id].data {
                BlockSetEntryData::Regular { mesh } => {
                    push(Transform::new(pos.as_vec3(), blk.orientation()));
                    *mesh
                }
                BlockSetEntryData::Wheel { mesh, armature } => {
                    let w = self.physics.pos_to_wheel.get(&pos).unwrap();
                    let (start_axle, start_tire) = (w.prev_axle, w.prev_tire);
                    let (end_axle, end_tire) =
                        self.physics.vehicle_body.wheel_local_transform(w.handle);
                    let axle = start_axle.interpolate(&end_axle, interpolation);
                    let tire = start_tire.interpolate(&end_tire, interpolation);
                    push(Transform::new(pos.as_vec3(), blk.orientation()));
                    push(axle);
                    push(tire);
                    *mesh
                }
                BlockSetEntryData::Weapon {
                    mesh,
                    armature,
                    projectile_mesh,
                    projectile_armature,
                } => {
                    let turret = self.turrets.get(&pos).unwrap();
                    let t = turret::turret_model_to_world_transform(
                        state,
                        &trf,
                        pos,
                        blk.orientation(),
                        *armature,
                        turret.pan,
                        turret.tilt,
                        true,
                    );
                    t.map(|t| trfs.push(t.into()));
                    *mesh
                }
            };
            draw.collector
                .solid
                .push(state.block_set.mesh_set(), mesh, 0, &trfs)
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
        state: &crate::State,
        physics: &Physics,
        controls: &InputControls,
    ) -> Vec<Projectile> {
        // movement
        let com = self.physics.center_of_mass(physics);

        let wheel_max_torque = 1000.0;
        let wheel_max_angle = core::f32::consts::FRAC_PI_6;
        let forward = controls.forward.clamp(-1.0, 1.0);
        let brake = f32::from(forward == 0.0);
        let pan = controls.pan.clamp(-1.0, 1.0);

        for (&pos, w) in self.physics.pos_to_wheel.iter() {
            //let forward = if pos.x < 0 { -forward } else { forward };
            let forward = if pos.y < 0 { -forward } else { forward };
            let pan = if (pos.x as f32) < com.x { -pan } else { pan };
            self.physics
                .vehicle_body
                .set_wheel_angle(w.handle, pan * wheel_max_angle);
            self.physics
                .vehicle_body
                .set_wheel_torque(w.handle, forward * wheel_max_torque);
            self.physics.vehicle_body.set_wheel_brake(w.handle, brake);
        }

        // aim target
        let trf = self.physics.transform(physics);
        let target = trf.apply_to_translation_inv(controls.target);

        for (&pos, turret) in self.turrets.iter_mut() {
            let blk = self.voxels.get(pos).unwrap();
            let trf = Transform::new(pos.as_vec3(), blk.orientation());
            let target = trf.apply_to_translation_inv(target);

            let BlockSetEntryData::Weapon { armature, .. } = &state.block_set[blk.id].data else {
                unreachable!()
            };
            let armature = &state.collection.armatures[*armature as usize];
            let trfs = armature.apply(&Transform::IDENTITY, &[Transform::IDENTITY; 3], false);
            let tilt_joint_offset = trfs[2].translation.xz();

            (turret.pan, turret.tilt) = turret::calc_pan_tilt(target, tilt_joint_offset);
        }

        // fire
        let mut projectiles = Vec::new();
        if controls.fire {
            let t = Instant::now();
            for (&pos, turret) in self.turrets.iter_mut() {
                if turret.next_fire > t {
                    continue;
                }

                let blk = self.voxels.get(pos).unwrap();

                let BlockSetEntryData::Weapon {
                    mesh: _,
                    armature,
                    projectile_mesh,
                    projectile_armature,
                } = &state.block_set[blk.id].data
                else {
                    unreachable!()
                };

                let turret_trfs = turret::turret_model_to_world_transform(
                    state,
                    &trf,
                    pos,
                    blk.orientation(),
                    *armature,
                    turret.pan,
                    turret.tilt,
                    false,
                );
                let muzzle_trf = turret_trfs[2];
                let dir = muzzle_trf.apply_to_direction(Vec3::Y);
                let ray = physics
                    .engine
                    .cast_ray(muzzle_trf.translation, dir * 1e3, None);
                let dist = ray.map_or(1e3, |r| r.distance);
                projectiles.push(Projectile {
                    start: muzzle_trf,
                    length: dist,
                    mesh_index: *projectile_mesh,
                    armature_index: *projectile_armature,
                });

                turret.next_fire = t.checked_add(Duration::from_secs(1)).unwrap();
            }
        }

        projectiles
    }

    pub fn destroy(mut self, physics: &mut Physics) {
        self.physics.destroy(physics);
    }
}

/// Damage stuff
impl Vehicle {
    pub fn apply_damage(
        &mut self,
        physics: &mut Physics,
        sound: &mut VehicleSound,
        acc: DamageAccumulator,
    ) {
        let destroyed = acc.apply(&mut self.damage);
        for pos in destroyed {
            self.physics.remove(physics, sound, pos);
        }
    }

    pub fn reset_damage(&mut self, block_set: &BlockSet) {
        self.damage.reset(block_set)
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
            out(block_set[b.id].name.as_bytes())?;
            out(b" ")?;
            out(b.orientation.to_string().as_bytes())?;
            out(b"\n")?;
        }
        Ok(())
    }

    pub fn load_v0_text(
        &mut self,
        sound: &mut VehicleSound,
        block_set: &BlockSet,
        physics: &mut Physics,
        text: &str,
    ) {
        self.voxels.clear();
        self.physics.clear(physics);
        self.turrets.clear();
        self.damage.clear();

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
            self.force_add(physics, sound, block_set, IVec3::new(x, y, z), blk);
        }
    }

    pub fn new_v0_text(
        block_set: &BlockSet,
        physics: &mut Physics,
        sound: &mut VehicleSound,
        text: &str,
        user_data: u32,
    ) -> Self {
        let mut slf = Self::new(physics, user_data);
        slf.load_v0_text(sound, block_set, physics, text);
        slf
    }
}
