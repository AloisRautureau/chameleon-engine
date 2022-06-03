use std::collections::VecDeque;
use crate::r#move::Move;
use crate::evaluation::Score;

pub const MAX_MOVELIST_CAPACITY: usize = 256;

#[derive(Clone, Copy)]
pub struct MoveList {
    moves: [Move; MAX_MOVELIST_CAPACITY],
    len: usize
}
impl Default for MoveList {
    fn default() -> Self {
        MoveList {
            moves: [Move::NULL_MOVE; 256],
            len: 0
        }
    }
}
impl From<Vec<Move>> for MoveList {
    fn from(v: Vec<Move>) -> Self {
        let mut mv_list = MoveList::default();
        for m in v { mv_list.push(m) }
        mv_list
    }
}
impl From<VecDeque<Move>> for MoveList {
    fn from(v: VecDeque<Move>) -> Self {
        let mut mv_list = MoveList::default();
        for m in v { mv_list.push(m) }
        mv_list
    }
}
impl MoveList {
    pub fn is_empty(&self) -> bool { self.len == 0 }
    pub fn len(&self) -> usize { self.len }
    pub fn push(&mut self, m: Move) { 
        self.moves[self.len] = m;
        self.len += 1;
    } 
    pub fn get(&self, i: usize) -> Option<Move> {
        if i >= self.len { None }
        else { Some(self.moves[i]) }
    }
    pub fn pop_index(&mut self, i: usize) -> Option<Move> {
        let m = self.get(i);
        self.len -= 1;
        self.moves[i] = self.moves[self.len];
        m
    }

    pub fn best_first_iter<F: Fn(Move) -> Score>(&self, scoring_function: &F) -> ScoredMoveListIterator {
        ScoredMoveListIterator::new(*self, scoring_function)
    }
}
impl IntoIterator for MoveList {
    type Item = Move;
    type IntoIter = MoveListIterator;

    fn into_iter(self) -> Self::IntoIter {
        MoveListIterator::new(self)
    }
}
impl std::fmt::Display for MoveList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut s = String::new();
        for m in self.into_iter() {
            s.push_str(&format!("{} ", m))
        }
        write!(f, "{}", s.trim())
    }
}

// Basic array iterator
pub struct MoveListIterator {
    inner: MoveList,
    idx: usize
}
impl MoveListIterator {
    fn new(inner: MoveList) -> Self {
        MoveListIterator {
            inner,
            idx: 0
        }
    }
}
impl Iterator for MoveListIterator {
    type Item = Move; 

    fn next(&mut self) -> Option<Self::Item> {
        self.idx += 1;
        self.inner.get(self.idx - 1)
    }
}

// A way to iterate through a movelist while scoring
// moves, to potentially reduce search space
pub struct ScoredMoveListIterator {
    inner: MoveList,
    scores: [Score; MAX_MOVELIST_CAPACITY]
}
impl ScoredMoveListIterator {
    pub fn new<F: Fn(Move) -> Score>(moves: MoveList, scoring_function: &F) -> ScoredMoveListIterator {
        let mut scores = [0; MAX_MOVELIST_CAPACITY];
        for i in 0..moves.len() {
            scores[i] = scoring_function(moves.get(i).unwrap());
        }
        ScoredMoveListIterator {
            inner: moves,
            scores
        }
    }
}
impl Iterator for ScoredMoveListIterator {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.inner.len() == 0 { None }
        else {
            let mut ix = 0;
            let mut s = -i32::MAX;
    
            for i in 0..self.inner.len() {
                if self.scores[i] > s {
                    s = self.scores[i];
                    ix = i;
                }
            }
            
            self.scores[ix] = self.scores[self.inner.len() - 1];
            self.inner.pop_index(ix)
        }
    }
}