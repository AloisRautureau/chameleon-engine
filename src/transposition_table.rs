use crate::{evaluation::Score, r#move::Move, zob_hash::Hash};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy)]
pub enum SearchInfo {
    Exact {
        position_hash: Hash,
        best_move: Move,
        depth_searched: u8,
        score: Score,
    },
    Cutoff {
        position_hash: Hash,
        refutation_move: Move,
        depth_searched: u8,
        high_bound: Score,
    },
    All {
        position_hash: Hash,
        best_move: Move,
        depth_searched: u8,
        low_bound: Score,
    },
    None,
}
impl SearchInfo {
    pub fn position_hash(&self) -> Option<Hash> {
        match self {
            Self::Exact {
                position_hash: h, ..
            } => Some(*h),
            Self::Cutoff {
                position_hash: h, ..
            } => Some(*h),
            Self::All {
                position_hash: h, ..
            } => Some(*h),
            Self::None => None,
        }
    }

    pub fn hash_move(&self) -> Option<Move> {
        match self {
            Self::Exact { best_move: m, .. } => Some(*m),
            Self::Cutoff {
                refutation_move: m, ..
            } => Some(*m),
            Self::All { best_move: m, .. } => Some(*m),
            _ => None,
        }
    }

    pub fn depth_searched(&self) -> Option<u8> {
        match self {
            Self::Exact {
                depth_searched: d, ..
            } => Some(*d),
            Self::Cutoff {
                depth_searched: d, ..
            } => Some(*d),
            Self::All {
                depth_searched: d, ..
            } => Some(*d),
            Self::None => None,
        }
    }

    pub fn bound(&self) -> Option<Score> {
        match self {
            Self::Exact { score: b, .. } => Some(*b),
            Self::Cutoff { high_bound: b, .. } => Some(*b),
            Self::All { low_bound: b, .. } => Some(*b),
            Self::None => None,
        }
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

    pub fn get(&self, hash: Hash) -> SearchInfo {
        let ptr = self.0.get(hash);
        let lock = ptr.lock().unwrap();
        let (depth_tier, young_tier) = *lock;

        if depth_tier.position_hash() == Some(hash) {
            depth_tier
        } else if young_tier.position_hash() == Some(hash) {
            young_tier
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

    /// Returns the best move from an exact entry if it exists for the
    /// given hash
    pub fn pv_move(&self, hash: Hash) -> Option<Move> {
        let ptr = self.0.get(hash);
        let lock = ptr.lock().unwrap();
        let (depth_tier, young_tier) = *lock;

        if let SearchInfo::Exact {
            best_move: m,
            position_hash: h,
            ..
        } = depth_tier
        {
            if h == hash {
                Some(m)
            } else {
                None
            }
        } else if let SearchInfo::Exact {
            best_move: m,
            position_hash: h,
            ..
        } = young_tier
        {
            if h == hash {
                Some(m)
            } else {
                None
            }
        } else {
            None
        }
    }

    // Prioritize:
    // - Exact > Cutoff > All
    // - depth otherwise
    fn should_replace(old_info: &SearchInfo, new_info: &SearchInfo) -> bool {
        old_info.depth_searched() < new_info.depth_searched()
    }
}
impl Default for TranspositionTable {
    fn default() -> Self {
        Self::new()
    }
}
