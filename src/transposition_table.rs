use crate::{evaluation::Score, r#move::Move, zob_hash::Hash};
use arrayvec::ArrayVec;
use spin::mutex::SpinMutex;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum NodeType {
    Exact,
    Alpha,
    Beta,
}
#[derive(Clone, Copy)]
pub struct SearchInfo {
    pub position_hash: Hash,
    pub best_move: Option<Move>,
    pub depth_searched: i32,
    pub score: Score,
    pub node_type: NodeType,
    pub age: bool,
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

pub const HASHTABLE_SIZE: usize = 2_usize.pow(16);

// A shareable, thread safe, but lockless hashtable.
// Instead, the locks are held by the entries
pub struct HashTable<T: Copy + Sync>(ArrayVec<SpinMutex<Option<T>>, HASHTABLE_SIZE>);
impl<T: Copy + Sync> HashTable<T> {
    // Creating a hashtable already fills every single entry
    pub fn new() -> Self {
        let mut table = ArrayVec::new();
        for _ in 0..HASHTABLE_SIZE {
            table.push(SpinMutex::new(None))
        }
        HashTable(table)
    }

    /// Returns the lock to a given entry
    pub fn get(&self, hash: Hash) -> &SpinMutex<Option<T>> {
        let key = self.key_from_hash(&hash);
        &self.0[key]
    }

    pub fn key_from_hash(&self, hash: &Hash) -> usize {
        (hash % (HASHTABLE_SIZE as u64)) as usize
    }
}

pub struct TranspositionTable(HashTable<(SearchInfo, Option<SearchInfo>)>);
impl TranspositionTable {
    pub fn new() -> Self {
        TranspositionTable(HashTable::new())
    }

    pub fn get(&self, hash: Hash) -> Option<SearchInfo> {
        let entry = *self.0.get(hash).lock();

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
        let entries = &mut *self.0.get(hash).lock();

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

    fn should_replace(old_info: &SearchInfo, new_info: &SearchInfo) -> bool {
        old_info.age != new_info.age || old_info.depth_searched <= new_info.depth_searched
    }
}
impl Default for TranspositionTable {
    fn default() -> Self {
        Self::new()
    }
}
