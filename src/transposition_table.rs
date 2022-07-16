use crate::{evaluation::Score, r#move::Move, zob_hash::Hash};
use std::sync::{Arc, Mutex};

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum SearchInfo {
    Exact {
        position_hash: Hash,
        score: Score,
        best_move: Move,
        depth_searched: u8,
        ply: u8,
    },
    Cutoff {
        position_hash: Hash,
        lower_bound: Score,
        refutation_move: Move,
        depth_searched: u8,
        ply: u8,
    },
    All {
        position_hash: Hash,
        higher_bound: Score,
        best_move: Move,
        depth_searched: u8,
        ply: u8,
    },
    None,
}
impl SearchInfo {
    pub fn position_hash(&self) -> Option<Hash> {
        match self {
            Self::Exact { position_hash, .. } => Some(*position_hash),
            Self::Cutoff { position_hash, .. } => Some(*position_hash),
            Self::All { position_hash, .. } => Some(*position_hash),
            Self::None => None,
        }
    }

    pub fn hash_move(&self) -> Option<Move> {
        match self {
            Self::Exact { best_move, .. } => Some(*best_move),
            Self::Cutoff {
                refutation_move, ..
            } => Some(*refutation_move),
            Self::All { best_move, .. } => Some(*best_move),
            _ => None,
        }
    }

    pub fn depth_searched(&self) -> Option<u8> {
        match self {
            Self::Exact { depth_searched, .. } => Some(*depth_searched),
            Self::Cutoff { depth_searched, .. } => Some(*depth_searched),
            Self::All { depth_searched, .. } => Some(*depth_searched),
            Self::None => None,
        }
    }

    pub fn bound(&self) -> Option<Score> {
        match self {
            Self::Exact { score, .. } => Some(*score),
            Self::Cutoff { lower_bound, .. } => Some(*lower_bound),
            Self::All { higher_bound, .. } => Some(*higher_bound),
            Self::None => None,
        }
    }

    pub fn last_probed_ply(&self) -> Option<u8> {
        match self {
            Self::Exact { ply, .. } => Some(*ply),
            Self::Cutoff { ply, .. } => Some(*ply),
            Self::All { ply, .. } => Some(*ply),
            Self::None => None,
        }
    }

    pub fn update_ply(&mut self, probed_ply: u32) {
        let probed_ply = probed_ply as u8;
        match self {
            Self::Exact { ply, .. } => *ply = std::cmp::max(probed_ply, *ply),
            Self::Cutoff { ply, .. } => *ply = std::cmp::max(probed_ply, *ply),
            Self::All { ply, .. } => *ply = std::cmp::max(probed_ply, *ply),
            Self::None => (),
        }
    }

    pub fn is_pv_node(&self) -> bool {
        matches!(self, SearchInfo::Exact { .. })
    }
}
impl Default for SearchInfo {
    fn default() -> Self {
        SearchInfo::None
    }
}

// A shareable, thread safe, but lockless hashtable.
// Instead, the locks are held by the entries
pub struct HashTable<T: Copy + Sync + Default>(Vec<Arc<Mutex<T>>>);
impl<T: Copy + Sync + Default> HashTable<T> {
    // Creating a hashtable already fills every single entry
    pub fn new(size: usize) -> Self {
        let mut table = Vec::with_capacity(size);
        for _ in 0..size {
            table.push(Arc::new(Mutex::new(Default::default())))
        }
        HashTable(table)
    }

    /// Returns an atomic reference to the entry (avoids locking the entire table)
    pub fn get(&self, hash: Hash) -> Arc<Mutex<T>> {
        let key = self.key_from_hash(&hash);
        self.0[key].clone()
    }

    pub fn key_from_hash(&self, hash: &Hash) -> usize {
        (hash % (self.0.capacity() as u64)) as usize
    }
}

pub struct TranspositionTable(HashTable<(SearchInfo, SearchInfo)>);
impl TranspositionTable {
    pub fn new() -> Self {
        TranspositionTable(HashTable::new(2_usize.pow(20) - 1))
    }

    pub fn get(&self, hash: Hash, ply: u32) -> SearchInfo {
        let ptr = self.0.get(hash);
        let mut lock = ptr.lock().unwrap();
        let (depth_tier, young_tier) = &mut *lock;

        if depth_tier.position_hash() == Some(hash) {
            depth_tier.update_ply(ply);
            *depth_tier
        } else if young_tier.position_hash() == Some(hash) {
            young_tier.update_ply(ply);
            *young_tier
        } else {
            SearchInfo::None
        }
    }

    pub fn set(&self, hash: Hash, entry: SearchInfo) {
        let ptr = self.0.get(hash);
        let mut lock = ptr.lock().unwrap();
        let (depth_tier, young_tier) = &mut *lock;

        if Self::should_replace(depth_tier, &entry) {
            *depth_tier = entry;
        } else {
            *young_tier = entry;
        }
    }

    fn should_replace(old_info: &SearchInfo, new_info: &SearchInfo) -> bool {
        let by_age = new_info
            .last_probed_ply()
            .map(|x| match old_info.last_probed_ply() {
                Some(y) => x - y,
                None => u8::MAX,
            })
            >= Some(8);
        let by_depth = old_info.depth_searched() < new_info.depth_searched();
        by_age || by_depth
    }
}
impl Default for TranspositionTable {
    fn default() -> Self {
        Self::new()
    }
}
