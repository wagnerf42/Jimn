use rand::Rng;
use std::cmp::Ordering::{Less, Greater};
use super::{Counting, Counter, RawTreap, Node, CTreap};

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
    /// is given key compatible with our limit in given direction ?
    fn fits_limit(&self, direction: usize, key: &K) -> bool {
        let constraints = [Greater, Less];
        if let Some(ref limit) = self.range[direction] {
            limit.cmp(key) != constraints[direction]
        } else {
            true
        }
    }

    /// Is given key compatible with our limit in given direction ?
    /// If there is no key, we need to have no limit.
    fn fits_unbounded_limit(&self, direction: usize, key: &Option<K>) -> bool {
        if let Some(ref real_key) = *key {
            self.fits_limit(direction, real_key)
        } else {
            self.range[direction].is_none()
        }
    }

    /// Do we contain given key ?
    fn contains(&self, key: &K) -> bool {
        for (limit, constraint) in self.range.iter().zip([Greater, Less].iter()) {
            if let Some(ref real_limit) = *limit {
                if real_limit.cmp(key) == *constraint {
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

impl<'a, K: 'a + Ord, V: 'a, R: 'a + Rng> ExactSizeIterator
    for OrderedIterator<'a, K, V, Counter, R> {
    fn len(&self) -> usize {
        // loop on the whole stack
        // for each node, if boolean (already seen) is 0, count the whole subtree
        // if boolean is 1 count node + right subtree
        let mut count = 0;
        for &(ref node, already_seen) in &self.remaining_nodes {
            if already_seen {
                unimplemented!();
            } else {
                unimplemented!();
                //count += self.treap.count(node, &self.limits, )
            }
        }
        count
    }
}

impl<'a, K: 'a + Ord + Clone + Copy, V: 'a> CTreap<'a, K, V> {
    /// Return the number of nodes between given bounds in subtree at given node.
    /// Current range limits possible values in given subtree.
    pub fn count(&self,
                 root: &Node<V, Counter>,
                 bounds: &KeyRange<K>,
                 current_range: KeyRange<K>)
                 -> usize {
        let included = current_range
            .range
            .iter()
            .enumerate()
            .map(|(d, k)| bounds.fits_unbounded_limit(d, k))
            .fold(true, |b1, b2| b1 && b2);
        if included {
            // subtree is fully included in given limits
            root.counter.0 as usize
        } else if !bounds.fits_unbounded_limit(0, &current_range.range[1]) ||
                  !bounds.fits_unbounded_limit(1, &current_range.range[0]) {
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
        assert!(r.fits_limit(0, &3));
        assert!(r.fits_limit(1, &5));
        let r = KeyRange { range: [Some(3), None] };
        assert!(r.fits_limit(1, &100));
    }
}
