use crate::{evaluation::Score, r#move::Move, zob_hash::Hash};
use std::sync::{Mutex, Arc};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum NodeType {
    Exact,
    Cutoff,
    All
}
#[derive(Clone, Copy)]
pub struct SearchInfo {
    pub position_hash: Hash,
    pub best_move: Option<Move>,
    pub depth_searched: i32,
    pub score: Score,
    pub node_type: NodeType,
}
impl std::fmt::Display for SearchInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "hash {:#0x}:\nbest {}, depth {}, score {} ({:#?} node)",
            self.position_hash,
            self.best_move.unwrap_or(Move::NULL_MOVE),
            self.depth_searched,
            self.score,
            self.node_type
        )
    }
}

// A shareable, thread safe, but lockless hashtable.
// Instead, the locks are held by the entries
pub struct HashTable<T: Copy + Sync>(Vec<Arc<Mutex<Option<T>>>>);
impl<T: Copy + Sync> HashTable<T> {
    // Creating a hashtable already fills every single entry
    pub fn new(size: usize) -> Self {
        let mut table = Vec::with_capacity(size);
        for _ in 0..size {
            table.push(Arc::new(Mutex::new(None)))
        }
        HashTable(table)
    }

    /// Returns an atomic reference to the entry (avoids locking the entire table)
    pub fn get(&self, hash: Hash) -> Arc<Mutex<Option<T>>> {
        let key = self.key_from_hash(&hash);
        self.0[key].clone()
    }

    pub fn key_from_hash(&self, hash: &Hash) -> usize {
        (hash % (self.0.capacity() as u64)) as usize
    }
}

pub struct TranspositionTable(HashTable<(SearchInfo, Option<SearchInfo>)>);
impl TranspositionTable {
    pub fn new() -> Self {
        TranspositionTable(HashTable::new(2_usize.pow(20) - 1))
    }

    pub fn get(&self, hash: Hash) -> Option<SearchInfo> {
        let ptr = self.0.get(hash);
        let lock = ptr.lock().unwrap();
        let entry = *lock;

        let (depth_entry, young_entry) = entry?;

        if depth_entry.position_hash == hash {
            return Some(depth_entry);
        } else if let Some(e) = young_entry {
            if e.position_hash == hash {
                return young_entry;
            }
        }
        None
    }

    pub fn set(&self, hash: Hash, entry: SearchInfo) {
        let ptr = self.0.get(hash);
        let mut lock = ptr.lock().unwrap();
        let entries = &mut *lock;

        let (depth_entry, young_entry) = if let Some(e) = entries {
            e
        } else {
            *entries = Some((entry, None));
            return;
        };

        if Self::should_replace(depth_entry, &entry) {
            *depth_entry = entry;
        } else {
            *young_entry = Some(entry);
        }
    }

    pub fn get_hash_move(&self, hash: Hash) -> Option<Move> {
        if let Some(info) = self.get(hash)  {
            info.best_move
        } else {
            None
        }
    }

    fn should_replace(old_info: &SearchInfo, new_info: &SearchInfo) -> bool {
        old_info.depth_searched <= new_info.depth_searched
    }
}
impl Default for TranspositionTable {
    fn default() -> Self {
        Self::new()
    }
}
