use crate::evaluation::Score;
use crate::r#move::Move;
use arrayvec::ArrayVec;
use std::collections::VecDeque;

pub const MAX_MOVELIST_CAPACITY: usize = 255;

#[repr(C)]
#[derive(Clone)]
pub struct MoveList(ArrayVec<Move, MAX_MOVELIST_CAPACITY>);
impl Default for MoveList {
    fn default() -> Self {
        MoveList(ArrayVec::new())
    }
}
impl MoveList {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn push(&mut self, m: Move) {
        self.0.push(m)
    }
    pub fn get(&self, i: usize) -> Option<&Move> {
        self.0.get(i)
    }
    pub fn pop_index(&mut self, i: usize) -> Option<Move> {
        self.0.swap_pop(i)
    }

    pub fn best_first_iter<F: Fn(&Move) -> Score>(
        &self,
        scoring_function: &F,
    ) -> ScoredMoveListIter {
        ScoredMoveListIter::new(self, scoring_function)
    }

    pub fn iter(&self) -> MoveListIter {
        MoveListIter::new(self)
    }
}
impl From<Vec<Move>> for MoveList {
    fn from(v: Vec<Move>) -> Self {
        let mut mv_list = MoveList::default();
        for m in v {
            mv_list.push(m)
        }
        mv_list
    }
}
impl From<VecDeque<Move>> for MoveList {
    fn from(v: VecDeque<Move>) -> Self {
        let mut mv_list = MoveList::default();
        for m in v {
            mv_list.push(m)
        }
        mv_list
    }
}
impl std::fmt::Display for MoveList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut s = String::new();
        for m in self.0.iter() {
            s.push_str(&format!("{} ", m))
        }
        write!(f, "{}", s.trim())
    }
}
impl<'a> IntoIterator for &'a MoveList {
    type Item = &'a Move;
    type IntoIter = MoveListIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct MoveListIter<'a> {
    inner: &'a MoveList,
    ix: usize,
}
impl<'a> MoveListIter<'a> {
    pub fn new(move_list: &'a MoveList) -> Self {
        MoveListIter {
            inner: move_list,
            ix: 0,
        }
    }
}
impl<'a> Iterator for MoveListIter<'a> {
    type Item = &'a Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ix >= self.inner.len() {
            None
        } else {
            self.ix += 1;
            self.inner.get(self.ix - 1)
        }
    }
}

// A way to iterate through a movelist while scoring
// moves, to potentially reduce search space
pub struct ScoredMoveListIter<'a> {
    moves: ArrayVec<&'a Move, MAX_MOVELIST_CAPACITY>,
    scores: ArrayVec<Score, MAX_MOVELIST_CAPACITY>,
}
impl<'a> ScoredMoveListIter<'a> {
    pub fn new<F: Fn(&Move) -> Score>(move_list: &'a MoveList, scoring_function: &F) -> Self {
        let mut moves = ArrayVec::new();
        let mut scores = ArrayVec::new();
        for mv in move_list {
            let score = scoring_function(mv);
            scores.push(score);
            moves.push(mv);
        }
        ScoredMoveListIter { moves, scores }
    }
}
impl<'a> Iterator for ScoredMoveListIter<'a> {
    type Item = &'a Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.moves.is_empty() {
            return None;
        }
        let (mut best_index, mut best_score) = (0, self.scores[0]);
        for (i, s) in (&self.scores).into_iter().enumerate() {
            if *s > best_score {
                best_score = *s;
                best_index = i;
            }
        }

        self.scores.swap_pop(best_index);
        self.moves.swap_pop(best_index)
    }
}
