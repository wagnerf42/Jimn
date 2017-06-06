// Some subtrees-size counting genericity for treaps.
// Two variants : do or don't count.

use std::fmt::Display;
use std::ops::{Add, Sub};
use std;

/// Do not count how many nodes in subtrees.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct EmptyCounter();

impl Display for EmptyCounter {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl Add for EmptyCounter {
    type Output = EmptyCounter;
    fn add(self, _other: EmptyCounter) -> EmptyCounter {
        EmptyCounter()
    }
}

impl Sub for EmptyCounter {
    type Output = EmptyCounter;
    fn sub(self, _other: EmptyCounter) -> EmptyCounter {
        EmptyCounter()
    }
}

impl Default for EmptyCounter {
    fn default() -> Self {
        EmptyCounter()
    }
}

/// Do count how many nodes in subtrees.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Counter(pub usize);

impl Display for Counter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "/{}", self.0)
    }
}

impl Add for Counter {
    type Output = Counter;
    fn add(self, other: Counter) -> Counter {
        Counter(self.0 + other.0)
    }
}

impl Sub for Counter {
    type Output = Counter;
    fn sub(self, other: Counter) -> Counter {
        Counter(self.0 - other.0)
    }
}

impl Default for Counter {
    fn default() -> Self {
        Counter(1)
    }
}

/// Treaps hold counters. We provide several flavors to allow choosing counting
/// subtrees sizes (with overhead) or not counting them (no overhead).
/// A counter must implement all following traits.
pub trait Counting
    : Add<Output = Self> + Sub<Output = Self> + Eq + Default + Copy {
}
impl Counting for Counter {}
impl Counting for EmptyCounter {}
