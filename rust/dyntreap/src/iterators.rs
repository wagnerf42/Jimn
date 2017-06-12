use rand::Rng;
use std::cmp::Ordering::{Less, Greater};
use super::{Counting, Counter, RawTreap, Node};

/// we use keys intervals in order to constraint iterator to subranges.
pub struct KeyRange<K: Ord> {
    /// lower and upper bound (both excluded)
    pub range: [Option<K>; 2],
}

impl<K: Copy + Ord> Copy for KeyRange<K> {}
impl<K: Copy + Ord> Clone for KeyRange<K> {
    fn clone(&self) -> Self {
        KeyRange { range: [self.range[0], self.range[1]] }
    }
}


impl<K: Ord> KeyRange<K> {
    /// is given key strictly (not equal) compatible with our limit in given direction ?
    fn fits_limit(&self, direction: usize, key: &K) -> bool {
        let constraints = [Less, Greater];
        if let Some(ref limit) = self.range[direction] {
            limit.cmp(key) == constraints[direction]
        } else {
            true
        }
    }

    /// Is other strictly included in self ?
    fn contains_range(&self, other: &Self) -> bool {
        other
            .range
            .iter()
            .enumerate()
            .map(|(d, k)| self.fits_unbounded_limit(d, k))
            .fold(true, |b1, b2| b1 && b2)
    }

    /// Are the two ranges fully disjoint (strictly) ?
    fn is_disjoint_with(&self, other: &Self) -> bool {
        self.range
            .iter()
            .enumerate()
            .map(|(d, k)| if let Some(ref key) = *k {
                     !other.fits_limit(1 - d, key)
                 } else {
                     false
                 })
            .fold(false, |b1, b2| b1 || b2)
    }

    /// Is given key strictly compatible with our limit in given direction ?
    /// If there is no key, we need to have no limit.
    fn fits_unbounded_limit(&self, direction: usize, key: &Option<K>) -> bool {
        if let Some(ref real_key) = *key {
            self.fits_limit(direction, real_key)
        } else {
            self.range[direction].is_none()
        }
    }

    /// Do we strictly contain given key ?
    fn contains(&self, key: &K) -> bool {
        for (limit, constraint) in self.range.iter().zip([Less, Greater].iter()) {
            if let Some(ref real_limit) = *limit {
                if real_limit.cmp(key) != *constraint {
                    return false;
                }
            }
        }
        true
    }
}



// Iterator through nodes, in key order
pub struct OrderedIterator<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> {
    pub direction: usize,
    pub limits: KeyRange<K>,
    pub treap: &'a RawTreap<'a, K, V, C, R>,
    pub remaining_nodes: Vec<(&'a Box<Node<V, C>>, bool)>,
}

// ExactSizeIterator through nodes, in key order.
pub struct ExactIterator<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> {
    pub direction: usize,
    pub limits: KeyRange<K>,
    pub treap: &'a RawTreap<'a, K, V, Counter, R>,
    pub remaining_nodes: Vec<(&'a Box<Node<V, Counter>>, bool, KeyRange<K>)>,
}

impl<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> OrderedIterator<'a, K, V, C, R> {
    pub fn upper_bound(mut self, upper_bound: K) -> Self {
        self.limits.range[1] = Some(upper_bound);
        self
    }

    pub fn lower_bound(mut self, lower_bound: K) -> Self {
        self.limits.range[0] = Some(lower_bound);
        self
    }
}

impl<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> ExactIterator<'a, K, V, R> {
    pub fn upper_bound(mut self, upper_bound: K) -> Self {
        self.limits.range[1] = Some(upper_bound);
        self
    }

    pub fn lower_bound(mut self, lower_bound: K) -> Self {
        self.limits.range[0] = Some(lower_bound);
        self
    }
}


impl<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> Iterator
    for OrderedIterator<'a, K, V, C, R> {
    type Item = &'a Node<V, C>;
    fn next(&mut self) -> Option<&'a Node<V, C>> {
        if let Some((next_node, seen)) = self.remaining_nodes.pop() {
            let key = (self.treap.keys_generator)(&next_node.value);
            if seen {
                if let Some(ref child) = next_node.children[self.direction] {
                    if self.limits.fits_limit(self.direction, &key) {
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
                if let Some(ref child) = next_node.children[1 - self.direction] {
                    if self.limits.fits_limit(1 - self.direction, &key) {
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
                if let Some(ref child) = node.children[self.direction] {
                    let mut child_range = range;
                    child_range.range[1 - self.direction] = Some(key);
                    if !child_range.is_disjoint_with(&self.limits) {
                        // there might be something left there
                        self.remaining_nodes.push((child, false, child_range));
                    }
                }
                if self.limits.contains(&key) {
                    Some(node)
                } else {
                    self.next()
                }
            } else {
                self.remaining_nodes.push((node, true, range));
                if let Some(ref child) = node.children[1 - self.direction] {
                    let mut child_range = range;
                    child_range.range[self.direction] = Some(key);
                    if !child_range.is_disjoint_with(&self.limits) {
                        // there might be something left there
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
        for &(node, already_seen, range) in &self.remaining_nodes {
            if already_seen {
                // count subtree in direction
                let key = (self.treap.keys_generator)(&node.value);
                if self.limits.contains(&key) {
                    count += 1;
                }
                if let Some(ref child) = node.children[self.direction] {
                    let mut child_range = range;
                    child_range.range[1 - self.direction] = Some(key);
                    count += self.treap.count(child, &self.limits, child_range);
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
    pub(crate) fn count(&self,
                        root: &Node<V, Counter>,
                        bounds: &KeyRange<K>,
                        current_range: KeyRange<K>)
                        -> usize {
        if bounds.contains_range(&current_range) {
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
                    let mut remaining_range = current_range;
                    remaining_range.range[1 - direction] = Some(root_key);
                    count += self.count(real_child, bounds, remaining_range);
                }
            }
            count
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_fits_limit() {
        let r = KeyRange { range: [Some(3), Some(5)] };
        assert!(r.fits_limit(0, &4));
        assert!(r.fits_limit(1, &4));
        assert!(r.fits_limit(0, &8));
        assert!(!r.fits_limit(1, &8));
        assert!(r.fits_limit(1, &1));
        assert!(!r.fits_limit(0, &1));
        assert!(!r.fits_limit(0, &3));
        assert!(!r.fits_limit(1, &5));
        let r = KeyRange { range: [Some(3), None] };
        assert!(r.fits_limit(1, &100));
    }

    #[test]
    fn test_contains_range() {
        let r = KeyRange { range: [Some(3), Some(7)] };
        let r2 = KeyRange { range: [Some(5), Some(6)] };
        assert!(r.contains_range(&r2));
        let r2 = KeyRange { range: [Some(3), Some(7)] };
        assert!(!r.contains_range(&r2));
        let r2 = KeyRange { range: [Some(1), Some(6)] };
        assert!(!r.contains_range(&r2));
        let r2 = KeyRange { range: [Some(5), Some(8)] };
        assert!(!r.contains_range(&r2));
        let r = KeyRange { range: [None, Some(7)] };
        let r2 = KeyRange { range: [Some(5), Some(6)] };
        assert!(r.contains_range(&r2));
        let r2 = KeyRange { range: [Some(5), Some(8)] };
        assert!(!r.contains_range(&r2));
    }

    #[test]
    fn test_is_disjoint() {
        let r = KeyRange { range: [Some(3), Some(8)] };
        let r2 = KeyRange { range: [Some(9), Some(12)] };
        assert!(r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [Some(1), Some(2)] };
        assert!(r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [None, Some(2)] };
        assert!(r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [Some(8), Some(12)] };
        assert!(r.is_disjoint_with(&r2));
        let r = KeyRange { range: [None, Some(8)] };
        let r2 = KeyRange { range: [Some(9), Some(12)] };
        assert!(r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [Some(7), Some(12)] };
        assert!(!r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [Some(12), None] };
        assert!(r.is_disjoint_with(&r2));
        let r = KeyRange { range: [None, None] };
        let r2 = KeyRange { range: [Some(3), Some(7)] };
        assert!(!r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [None, Some(7)] };
        assert!(!r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [Some(3), None] };
        assert!(!r.is_disjoint_with(&r2));
        let r2 = KeyRange { range: [None, None] };
        assert!(!r.is_disjoint_with(&r2));
    }
}
