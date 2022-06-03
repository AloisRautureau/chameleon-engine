use std::collections::HashMap;
use crate::{zob_hash::Hash, evaluation::Score, r#move::Move};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum NodeType {
    Exact, Alpha, Beta
}
#[derive(Clone, Copy)]
pub struct SearchInfo {
    pub position_hash: Hash,
    pub best_move: Option<Move>,
    pub depth_searched: u32,
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

pub struct TranspositionTable (HashMap<Hash, (SearchInfo, Option<SearchInfo>)>);

//TODO: Maybe use rustc-hash crate if hashing ever becomes hot
impl TranspositionTable {
    pub fn new(entries: usize) -> TranspositionTable {
        TranspositionTable (HashMap::with_capacity(entries))
    }

    pub fn get(&self, hash: Hash) -> Option<SearchInfo> {
        if let Some((d,oa)) = self.0.get(&self.key_from_hash(hash)) {
            if d.position_hash == hash { return Some(*d) } 
            else if let Some(a) = oa { if a.position_hash == hash { return Some(*a) }}
        }
        None
    }

    pub fn set(&mut self, hash: Hash, entry: SearchInfo) {
        match self.0.get_mut(&self.key_from_hash(hash)) {
            Some((d, oa)) => {
                if Self::should_replace(&d, &entry) {
                    *d = entry;
                } else {
                    *oa = Some(entry)
                }
            },
            None => { self.0.insert(self.key_from_hash(hash), (entry, None)); }
        }
    }

    fn should_replace(old_info: &SearchInfo, new_info: &SearchInfo) -> bool {
        old_info.age != new_info.age || old_info.depth_searched <= new_info.depth_searched
    }

    fn key_from_hash(&self, hash: Hash) -> Hash {
        hash % (self.0.capacity() as u64)
    }
}
