//! Define some range intervals for searches.
use std::cmp::Ordering::*;
use std::collections::Bound;
use std::collections::Bound::*;
use std::collections::range::RangeArgument;

use super::{DECREASING, INCREASING};

// Store some keys interval.
// with some methods for code clarity.
#[derive(Debug)]
pub struct KeyRange<K>(pub [Bound<K>; 2]);

impl<K: Ord> KeyRange<K> {
    /// is given key strictly compatible with our limit in given direction ?
    pub fn fits_limit(&self, direction: usize, key: &K) -> bool {
        let constraint = if direction == DECREASING {
            Greater // key needs to be more than our lower limit
        } else {
            Less // key needs to be less than our higher limit
        };
        match self.0[direction] {
            Unbounded => true,
            Included(ref limit) => {
                let comparison = key.cmp(limit);
                comparison == constraint || comparison == Equal
            }
            Excluded(ref limit) => key.cmp(limit) == constraint,
        }
    }

    /// Is other strictly included in self ?
    pub fn contains_range(&self, other: &Self) -> bool {
        let low_ok: bool = match self.0[0] {
            Unbounded => true,
            Included(ref low) => {
                match other.0[0] {
                    Unbounded => false,
                    Included(ref other_low) |
                    Excluded(ref other_low) => other_low.ge(low),
                }
            }
            Excluded(ref low) => {
                match other.0[0] {
                    Unbounded => false,
                    Included(ref other_low) => other_low.gt(low),
                    Excluded(ref other_low) => other_low.ge(low),
                }
            }
        };
        low_ok &&
        match self.0[1] {
            Unbounded => true,
            Included(ref high) => {
                match other.0[1] {
                    Unbounded => false,
                    Included(ref other_high) |
                    Excluded(ref other_high) => other_high.le(high),
                }
            }
            Excluded(ref high) => {
                match other.0[1] {
                    Unbounded => false,
                    Included(ref other_high) => other_high.lt(high),
                    Excluded(ref other_high) => other_high.le(high),
                }
            }
        }
    }
    /// Are the two ranges fully disjoint (strictly) ?
    pub fn is_disjoint_with(&self, other: &Self) -> bool {
        let ok_low = match self.0[0] {
            Unbounded => false,
            Included(ref low) => {
                match other.0[1] {
                    Unbounded => false,
                    Included(ref other_high) => other_high.lt(low),
                    Excluded(ref other_high) => other_high.le(low),
                }
            }
            Excluded(ref low) => {
                match other.0[1] {
                    Unbounded => false,
                    Included(ref other_high) |
                    Excluded(ref other_high) => other_high.le(low),
                }
            }
        };
        ok_low ||
        match self.0[1] {
            Unbounded => false,
            Included(ref high) => {
                match other.0[0] {
                    Unbounded => false,
                    Included(ref other_low) => other_low.gt(high),
                    Excluded(ref other_low) => other_low.ge(high),
                }
            }
            Excluded(ref high) => {
                match other.0[0] {
                    Unbounded => false,
                    Included(ref other_low) |
                    Excluded(ref other_low) => other_low.ge(high),
                }
            }
        }
    }

    /// Do we contain given key ?
    pub fn contains(&self, key: &K) -> bool {
        [DECREASING, INCREASING]
            .into_iter()
            .map(|d| self.fits_limit(*d, key))
            .fold(true, |b1, b2| b1 && b2)
    }
}

impl<K: Copy> Clone for KeyRange<K> {
    fn clone(&self) -> Self {
        KeyRange([self.0[0], self.0[1]])
    }
}

impl<K: Copy> KeyRange<K> {
    /// Create a `KeyRange` out of given RangeArgument.
    pub fn new_from(range: &RangeArgument<K>) -> Self {
        KeyRange([match range.start() {
                      Unbounded => Unbounded,
                      Included(k) => Included(*k),
                      Excluded(k) => Excluded(*k),
                  },
                  match range.end() {
                      Unbounded => Unbounded,
                      Included(k) => Included(*k),
                      Excluded(k) => Excluded(*k),
                  }])
    }
}
