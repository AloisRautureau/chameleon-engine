use crate::evaluation::Score;
use crate::r#move::Move;

pub const MAX_MOVELIST_CAPACITY: usize = 255;

#[repr(C)]
#[derive(Clone)]
pub struct MoveList([Move; MAX_MOVELIST_CAPACITY], usize);
impl Default for MoveList {
    fn default() -> Self {
        MoveList([Move::NULL_MOVE; MAX_MOVELIST_CAPACITY], 0)
    }
}
impl MoveList {
    pub fn is_empty(&self) -> bool {
        self.1 == 0
    }
    pub fn len(&self) -> usize {
        self.1
    }
    pub fn push(&mut self, m: Move) {
        self.0[self.len()] = m;
        self.1 += 1;
    }
    pub fn get(&self, i: usize) -> Option<&Move> {
        if i < self.len() {
            Some(&self.0[i])
        } else {
            None
        }
    }
    pub fn swap_pop(&mut self, i: usize) -> Option<Move> {
        if i >= self.len() { return None }
        self.1 -= 1;
        self.0.swap(i, self.1);
        Some(self.0[self.1])
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
    moves: [&'a Move; MAX_MOVELIST_CAPACITY],
    scores: [Score; MAX_MOVELIST_CAPACITY],
    len: usize
}
impl<'a> ScoredMoveListIter<'a> {
    pub fn new<F: Fn(&Move) -> Score>(move_list: &'a MoveList, scoring_function: &F) -> Self {
        let mut moves = [&Move::NULL_MOVE; MAX_MOVELIST_CAPACITY];
        let mut scores = [0; MAX_MOVELIST_CAPACITY];
        for (i, m) in move_list.iter().enumerate() {
            moves[i] = m;
            scores[i] = scoring_function(m);
        }
        ScoredMoveListIter { 
            moves, 
            scores,
            len: move_list.len()
        }
    }
}
impl<'a> Iterator for ScoredMoveListIter<'a> {
    type Item = &'a Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        let (mut best_index, mut best_score) = (0, self.scores[0]);
        for (i, s) in self.scores.iter().take(self.len).enumerate() {
            if *s > best_score {
                best_score = *s;
                best_index = i;
            }
        }

        self.len -= 1;
        self.scores.swap(best_index, self.len);
        self.moves.swap(best_index, self.len);
        Some(self.moves[self.len])
    }
}
