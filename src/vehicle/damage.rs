use std::collections::{HashMap, HashSet, VecDeque};

use mechaia::math::IVec3;

use super::{Block, BlockSet};

/// Voxel body to assist with damage simulations.
pub struct Body {
    /// Mapping of voxel coordinates to index in blocks array.
    voxels: HashMap<IVec3, u32>,
    /// hitpoints, voxel coordinates, id.
    blocks: mechaia::util::soa::Vec3<u32, Box<[IVec3]>, u32>,
    /// Index of the core block of this vehicle.
    core: u32,
    /// Free slots in blocks list.
    ///
    /// NOTE: Only really used in the editor.
    free_blocks_slots: Vec<u32>,
}

/// "Damage" accumulator
///
/// Used to combine damage sources and process them in one go.
#[derive(Default)]
pub struct DamageAccumulator {
    /// "heat" damage.
    ///
    /// Damage will spread to nearest blocks first, using broad-first-search.
    heat: Vec<(IVec3, u32)>,
}

impl Body {
    /// Insert a new block with full hitpoints
    ///
    /// It check if
    /// - whether a block was already present. If so, the function panics.
    /// - the new block is a core. If a core is already present, the function panics.
    pub fn insert(&mut self, block_set: &BlockSet, pos: IVec3, block: Block) {
        let meta = &block_set[block.id];
        let value = (meta.health, Vec::from([pos]).into(), block.id);
        let i = if let Some(i) = self.free_blocks_slots.pop() {
            self.blocks.set(i.try_into().unwrap(), value);
            i
        } else {
            self.blocks.push(value);
            (self.blocks.len() - 1).try_into().unwrap()
        };
        let prev = self.voxels.insert(pos, i);
        assert!(prev.is_none());
        if Self::is_core(block_set, block.id) {
            assert!(!self.has_core());
            self.core = i;
        }
    }

    /// Remove a block.
    pub fn remove(&mut self, pos: IVec3) {
        let i = self.voxels.remove(&pos).expect("no block at position");
        self.free_blocks_slots.push(i);
        if i == self.core {
            self.core = u32::MAX;
        }
    }

    /// Reset damage, restoring back to full health.
    pub fn reset(&mut self, block_set: &BlockSet) {
        for (hp, _, id) in self.blocks.iter_mut() {
            *hp = block_set[*id].health;
        }
    }

    /// Clear entirely
    pub fn clear(&mut self) {
        self.voxels.clear();
        self.blocks.clear();
        self.core = u32::MAX;
        self.free_blocks_slots.clear();
    }

    /// Check if a core is set
    pub fn has_core(&self) -> bool {
        self.core != u32::MAX
    }

    /// Get a block's hitpoints.
    pub fn health(&self, pos: IVec3) -> u32 {
        let i = *self.voxels.get(&pos).unwrap();
        *self.blocks.get(usize::try_from(i).unwrap()).unwrap().0
    }

    /// Does a block qualify as core?
    pub fn is_core(block_set: &BlockSet, block_id: u32) -> bool {
        block_set[block_id].name.starts_with("core.")
    }
}

impl Default for Body {
    fn default() -> Self {
        Self {
            voxels: Default::default(),
            blocks: Default::default(),
            core: u32::MAX,
            free_blocks_slots: Default::default(),
        }
    }
}

impl DamageAccumulator {
    pub fn add_heat(&mut self, start: IVec3, damage: u32) {
        self.heat.push((start, damage));
    }

    /// Apply damage.
    pub fn apply(self, body: &mut Body) {
        for &(start, mut dmg) in &self.heat {
            crate::log::debug(format!("applying {dmg} heat"));
            let mut visit = VecDeque::from([start]);
            while let (true, Some(pos)) = (dmg > 0, visit.pop_front()) {
                let Some(&i) = body.voxels.get(&pos) else { continue };
                let blk = body.blocks.get_mut(usize::try_from(i).unwrap()).unwrap();

                if *blk.0 == 0 {
                    continue;
                }

                let hp = blk.0.saturating_sub(dmg);
                dmg -= *blk.0 - hp;
                crate::log::debug(format!("ouch {} -> {hp} (remaining: {dmg})", blk.0));
                *blk.0 = hp;

                if hp == 0 {
                    for &p in blk.1.iter() {
                        for d in [IVec3::X, IVec3::Y, IVec3::Z] {
                            visit.push_back(p + d);
                            visit.push_back(p - d);
                        }
                    }
                }
            }
        }

        self.destroy_disconnected(body)
    }

    fn destroy_disconnected(self, body: &mut Body) {
        // do a floodfill starting from the core
        // this has (much) worse average-case runtime, but better worst-case runtime,
        // so less likely to cause lagspikes
        // (it is also very simple)
        if body.core == u32::MAX {
            crate::log::warn("no core, won't attempt disconnection");
            return;
        }
        let core_positions = body.blocks.get(usize::try_from(body.core).unwrap()).unwrap().1;
        // collect indices instead of positions to
        // - save a bit of memory
        // - faster processing of block list (last loop)
        // it might bloat the queue though
        let mut fill = HashSet::from([body.core]);
        // both BFS (queue) and DFS (stack) work, but BFS will use less memory
        let mut queue = VecDeque::from([body.core]);
        while let Some(i) = queue.pop_front() {
            for &pos in body.blocks.get(usize::try_from(i).unwrap()).unwrap().1.iter() {
                for d in [IVec3::X, IVec3::Y, IVec3::Z, -IVec3::X, -IVec3::Y, -IVec3::Z] {
                    let v = pos + d;
                    let Some(&k) = body.voxels.get(&v) else { continue };
                    if fill.contains(&k) {
                        continue;
                    }
                    let blk = body.blocks.get(usize::try_from(k).unwrap()).unwrap();
                    if *blk.0 == 0 {
                        continue;
                    }
                    fill.insert(k);
                    queue.push_back(k);
                }
            }
        }
        
        for (i, (hp, _, _)) in body.blocks.iter_mut().enumerate() {
            if !fill.contains(&u32::try_from(i).unwrap()) {
                *hp = 0;
            }
        }
    }
}
