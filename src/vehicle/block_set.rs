use crate::{
    gfx::{Gfx, MeshSetHandle},
    Collection,
};
use core::ops::Index;
use std::collections::HashMap;

pub struct BlockSet {
    mesh_set: MeshSetHandle,
    blocks: Vec<BlockSetEntry>,
    name_to_id: HashMap<Box<str>, u32>,
}

pub struct BlockSetEntry {
    pub name: Box<str>,
    pub data: BlockSetEntryData,
    pub health: u32,
}

pub enum BlockSetEntryData {
    Regular {
        mesh: u32,
    },
    Wheel {
        mesh: u32,
        armature: u32,
    },
    Weapon {
        mesh: u32,
        armature: u32,
        projectile_mesh: u32,
        projectile_armature: u32,
    },
}

impl BlockSet {
    pub fn from_collection(gfx: &mut Gfx, collection: &Collection) -> Self {
        let mut blocks = Vec::new();
        let mut name_to_id = HashMap::new();

        // FIXME we need a better way to manage these damn assets

        // collect projectiles first
        let mut projectiles = HashMap::new();
        for root in collection.scenes.iter() {
            for node in root.descendants() {
                let Some(name) = node
                    .properties()
                    .name
                    .as_ref()
                    .filter(|n| n.starts_with("projectile."))
                else {
                    continue;
                };
                let name = &name[11..];
                crate::log::debug(format!("adding projectile {name}"));

                let mechaia::model::Node::Leaf { model, .. } = node else {
                    panic!("todo: handle multi-mesh projectiles (or not?)");
                };
                projectiles.insert(name, *model);
            }
        }

        // now collect blocks, including weapons
        for root in collection.scenes.iter() {
            for node in root.descendants() {
                let Some(name) = node
                    .properties()
                    .name
                    .as_ref()
                    .filter(|n| n.starts_with("block."))
                else {
                    continue;
                };
                let name = &name[6..];
                crate::log::debug(format!("adding block {name}"));

                let mechaia::model::Node::Leaf { model, .. } = node else {
                    panic!("todo: handle multi-mesh blocks (or not?)");
                };
                let model = &collection.models[*model];
                let mesh = model.mesh_index.try_into().unwrap();

                let data = if name.starts_with("wheel.") {
                    let armature = model.armature_index.try_into().unwrap();
                    BlockSetEntryData::Wheel { mesh, armature }
                } else if name.starts_with("weapon.turret.") {
                    let armature = model.armature_index.try_into().unwrap();
                    let Some(proj) = projectiles.get(&*name) else {
                        crate::log::error(format!("no projectile defined for {name}"));
                        continue;
                    };
                    let proj = &collection.models[*proj];
                    BlockSetEntryData::Weapon {
                        mesh,
                        armature,
                        projectile_mesh: proj.mesh_index.try_into().unwrap(),
                        projectile_armature: proj.armature_index.try_into().unwrap(),
                    }
                } else {
                    BlockSetEntryData::Regular { mesh }
                };

                let i = blocks.len().try_into().unwrap();
                let name = Box::<str>::from(name);
                blocks.push(BlockSetEntry {
                    name: name.clone(),
                    data,
                    health: 100,
                });
                name_to_id.insert(name, i);
            }
        }

        let mesh_set = gfx.add_mesh_set(collection);

        Self {
            mesh_set,
            blocks,
            name_to_id,
        }
    }

    pub fn mesh_set(&self) -> MeshSetHandle {
        self.mesh_set
    }

    pub fn cube_id(&self) -> u32 {
        self.get_id("cube").expect("no cube defined")
    }

    pub fn get_id(&self, name: &str) -> Option<u32> {
        self.name_to_id.get(name).copied()
    }

    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    pub fn len_u32(&self) -> u32 {
        self.len().try_into().unwrap()
    }
}

impl Index<u32> for BlockSet {
    type Output = BlockSetEntry;

    fn index(&self, id: u32) -> &Self::Output {
        &self.blocks[usize::try_from(id).unwrap()]
    }
}

impl BlockSetEntry {
    pub fn mesh(&self) -> u32 {
        match &self.data {
            BlockSetEntryData::Wheel { mesh, .. } => *mesh,
            BlockSetEntryData::Weapon { mesh, .. } => *mesh,
            BlockSetEntryData::Regular { mesh } => *mesh,
        }
    }
}
