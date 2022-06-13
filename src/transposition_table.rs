use std::sync::Mutex;
use crate::{zob_hash::Hash, evaluation::Score, r#move::Move};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum NodeType {
    Exact, Alpha, Beta
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
            self.position_hash, self.best_move.unwrap_or(Move::NULL_MOVE), self.depth_searched, self.score,
            self.node_type
        )
    }
}

pub struct TranspositionTable (Vec<Mutex<(Option<SearchInfo>, Option<SearchInfo>)>>);

impl TranspositionTable {
    pub fn new(entries: usize) -> TranspositionTable {
        let mut table = Vec::with_capacity(entries);
        for _ in 0..entries {
            table.push(Mutex::new((None, None)))
        }
        TranspositionTable (table)
    }

    pub fn get(&self, hash: Hash) -> Option<SearchInfo> {
        let (depth_entry, young_entry) = if let Some(e) = self.0.get(self.key_from_hash(hash)) {
            *e.lock().unwrap()
        } else {
            return None
        };
        if let Some(e) = depth_entry {
            if e.position_hash == hash { return depth_entry }
        } else if let Some(e) = young_entry {
            if e.position_hash == hash { return young_entry }
        }
        None
    }

    pub fn set(&self, hash: Hash, entry: SearchInfo) {
        let mut entries = if let Some(e) = self.0.get(self.key_from_hash(hash)) {
            e.lock().unwrap()
        } else {
            return;
        };

        let depth_entry = &mut (*entries).0;

        let place_in_depth = if let Some(e) = depth_entry {
            Self::should_replace(&e, &entry)
        } else {
            true
        };

        if place_in_depth {
            *depth_entry = Some(entry);
        } else {
            //drop(depth_entry);
            let young_entry = &mut (*entries).1;
            *young_entry = Some(entry);
        }
    }

    fn should_replace(old_info: &SearchInfo, new_info: &SearchInfo) -> bool {
        old_info.age != new_info.age || old_info.depth_searched <= new_info.depth_searched
    }

    fn key_from_hash(&self, hash: Hash) -> usize {
        (hash % (self.0.capacity() as u64)) as usize
    }
}
