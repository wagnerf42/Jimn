use rand::Rng;
use super::{Counting, Counter, RawTreap, Node};
use std::collections::Bound;
use std::collections::Bound::*;
use std::cmp::Ordering::*;
use super::{DECREASING, INCREASING};

// Store some keys interval.
// with some methods for code clarity.
#[derive(Debug)]
pub struct KeyRange<K>(pub [Bound<K>; 2]);

impl<K: Ord> KeyRange<K> {
    /// is given key strictly compatible with our limit in given direction ?
    fn fits_limit(&self, direction: usize, key: &K) -> bool {
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
    fn contains_range(&self, other: &Self) -> bool {
        //TODO: without this variable the compiler thinks || is a closure and not an or
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
    fn is_disjoint_with(&self, other: &Self) -> bool {
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
    fn contains(&self, key: &K) -> bool {
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

// Iterator through nodes, in key order
pub struct OrderedIterator<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> {
    pub limits: KeyRange<K>,
    pub treap: &'a RawTreap<'a, K, V, C, R>,
    pub remaining_nodes: Vec<(&'a Node<V, C>, bool)>,
}

// ExactSizeIterator through nodes, in key order.
// we do not factorize with OrderedIterator because it incurs some overhead.
// TODO: we could in fact factorize with the empty tuple trick.
pub struct ExactIterator<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> {
    pub limits: KeyRange<K>,
    pub treap: &'a RawTreap<'a, K, V, Counter, R>,
    pub remaining_nodes: Vec<(&'a Node<V, Counter>, bool, KeyRange<K>)>,
}

impl<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> Iterator
    for OrderedIterator<'a, K, V, C, R> {
    type Item = &'a Node<V, C>;
    fn next(&mut self) -> Option<&'a Node<V, C>> {
        if let Some((next_node, seen)) = self.remaining_nodes.pop() {
            let key = (self.treap.keys_generator)(&next_node.value);
            if seen {
                if let Some(ref child) = next_node.children[INCREASING] {
                    // if we are already too big, no need to go towards larger child
                    if self.limits.fits_limit(INCREASING, &key) {
                        self.remaining_nodes.push((child, false));
                    }
                }
                if self.limits.contains(&key) {
                    Some(next_node)
                } else {
                    self.next()
                }
            } else {
                self.remaining_nodes.push((next_node, true));
                if let Some(ref child) = next_node.children[DECREASING] {
                    // if we are already too small, no need to go towards smaller child
                    if self.limits.fits_limit(DECREASING, &key) {
                        self.remaining_nodes.push((child, false));
                    }
                }
                self.next()
            }
        } else {
            None
        }
    }
}

impl<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> Iterator for ExactIterator<'a, K, V, R> {
    type Item = &'a Node<V, Counter>;
    fn next(&mut self) -> Option<&'a Node<V, Counter>> {
        if let Some((node, seen, range)) = self.remaining_nodes.pop() {
            let key = (self.treap.keys_generator)(&node.value);
            if seen {
                if let Some(ref child) = node.children[INCREASING] {
                    if self.limits.fits_limit(INCREASING, &key) {
                        let mut child_range = range;
                        child_range.0[0] = Included(key);
                        self.remaining_nodes.push((child, false, child_range));
                    }
                }
                if self.limits.contains(&key) {
                    Some(node)
                } else {
                    self.next()
                }
            } else {
                self.remaining_nodes.push((node, true, range.clone()));
                if let Some(ref child) = node.children[DECREASING] {
                    if self.limits.fits_limit(DECREASING, &key) {
                        let mut child_range = range;
                        child_range.0[1] = Included(key);
                        self.remaining_nodes.push((child, false, child_range));
                    }
                }
                self.next()
            }
        } else {
            None
        }
    }
}


impl<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> ExactSizeIterator for ExactIterator<'a, K, V, R> {
    fn len(&self) -> usize {
        // loop on the whole stack
        // for each node, if boolean (already seen) is 0, count the whole subtree
        // if boolean is 1 count node + right subtree
        let mut count = 0;
        for &(node, already_seen, ref range) in &self.remaining_nodes {
            if already_seen {
                // count subtree in direction
                let key = (self.treap.keys_generator)(&node.value);

                if self.limits.contains(&key) {
                    count += 1;
                }
                if let Some(ref child) = node.children[INCREASING] {
                    let mut child_range = range.clone();
                    child_range.0[0] = Included(key);
                    count += self.treap.count(child, &self.limits, &child_range);
                }
            } else {
                // count full subtree
                count += self.treap.count(node, &self.limits, range);
            }
        }
        count
    }
}

impl<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> RawTreap<'a, K, V, Counter, R> {
    /// Return the number of nodes between given bounds in subtree at given node.
    /// Current range limits possible values in given subtree.
    pub(crate) fn count(
        &self,
        root: &Node<V, Counter>,
        bounds: &KeyRange<K>,
        current_range: &KeyRange<K>,
    ) -> usize {
        if bounds.contains_range(current_range) {
            // subtree is fully included in given limits
            root.counter.0 as usize
        } else if current_range.is_disjoint_with(bounds) {
            // if intersection is empty return 0
            0
        } else {
            // else count recursively
            let mut count = 0;
            let root_key = (self.keys_generator)(&root.value);
            if bounds.contains(&root_key) {
                count += 1;
            }
            for (direction, child) in root.children.iter().enumerate() {
                if let Some(ref real_child) = *child {
                    let mut remaining_range = current_range.clone();
                    remaining_range.0[1 - direction] = Included(root_key);
                    count += self.count(real_child, bounds, &remaining_range);
                }
            }
            count
        }
    }
}
