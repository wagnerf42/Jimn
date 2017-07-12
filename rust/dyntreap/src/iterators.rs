//! define a mighty in-order double-ended exactsize iterator
use std::iter::once;
use rand::Rng;
use super::{Counting, Counter, RawTreap, Node};
use std::collections::Bound;
use std::collections::Bound::*;
use std::collections::VecDeque;
use super::{DECREASING, INCREASING};
use super::ranges::KeyRange;


/// We will need to store what's left to explore of the tree.
pub enum Remaining<'a, V: 'a, C: 'a + Counting> {
    Subtree(&'a Node<V, C>),
    Node(&'a Node<V, C>),
}


// Double Ended Iterator through nodes, in key order
pub struct DoubleIterator<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> {
    pub limits: KeyRange<K>,
    pub treap: &'a RawTreap<'a, K, V, C, R>,
    pub remaining: VecDeque<Remaining<'a, V, C>>,
}

impl<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> Iterator
    for DoubleIterator<'a, K, V, C, R> {
    type Item = &'a Node<V, C>;
    fn next(&mut self) -> Option<&'a Node<V, C>> {
        if let Some(next_thing) = self.remaining.pop_front() {
            match next_thing {
                Remaining::Node(n) => {
                    let key = (self.treap.keys_generator)(&n.value);
                    if self.limits.contains(&key) {
                        Some(n)
                    } else {
                        self.next()
                    }
                }
                Remaining::Subtree(n) => {
                    let key = (self.treap.keys_generator)(&n.value);
                    if let Some(ref child) = n.children[INCREASING] {
                        if self.limits.fits_limit(INCREASING, &key) {
                            self.remaining.push_front(Remaining::Subtree(child));
                        }
                    }
                    self.remaining.push_front(Remaining::Node(n));
                    if let Some(ref child) = n.children[DECREASING] {
                        if self.limits.fits_limit(DECREASING, &key) {
                            self.remaining.push_front(Remaining::Subtree(child));
                        }
                    }
                    self.next()
                }
            }
        } else {
            None
        }
    }
}

impl<'a, K: 'a + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> DoubleEndedIterator
    for DoubleIterator<'a, K, V, C, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(next_thing) = self.remaining.pop_back() {
            match next_thing {
                Remaining::Node(n) => {
                    let key = (self.treap.keys_generator)(&n.value);
                    if self.limits.contains(&key) {
                        Some(n)
                    } else {
                        self.next_back()
                    }
                }
                Remaining::Subtree(n) => {
                    let key = (self.treap.keys_generator)(&n.value);
                    if let Some(ref child) = n.children[DECREASING] {
                        if self.limits.fits_limit(DECREASING, &key) {
                            self.remaining.push_back(Remaining::Subtree(child));
                        }
                    }
                    self.remaining.push_back(Remaining::Node(n));
                    if let Some(ref child) = n.children[INCREASING] {
                        if self.limits.fits_limit(INCREASING, &key) {
                            self.remaining.push_back(Remaining::Subtree(child));
                        }
                    }
                    self.next_back()
                }
            }
        } else {
            None
        }
    }
}

impl<'a, K: 'a + Ord + Copy, V: 'a, R: 'a + Rng> ExactSizeIterator
    for DoubleIterator<'a, K, V, Counter, R> {
    fn len(&self) -> usize {
        let mut size = 0;
        let mut subtree_range = KeyRange([Unbounded, Unbounded]);
        let mut untreated_subtree = None;

        //we loop on all remaining subtrees counting things inside them.
        //the trick is to avoid counting un-necessary stuff.
        //since node keys split subtrees keys into ranges we use it to avoid counting
        //everything.

        //we need to convert nodes to bounds iterate on a mixed flow of bounds and subtrees.
        enum TreeOrBound<'a, K: 'a, V: 'a, C: 'a + Counting> {
            Tree(&'a Node<V, C>),
            Bound(Bound<K>),
        }

        let mut nodes_count = 0;
        for tob in self.remaining
            .iter()
            .map(|remain| match *remain {
                Remaining::Subtree(t) => TreeOrBound::Tree(t),
                Remaining::Node(n) => {
                    let key = (self.treap.keys_generator)(&n.value);
                    if self.limits.contains(&key) {
                        nodes_count += 1;
                    }
                    TreeOrBound::Bound(Included(key))
                }
            })
            .chain(once(TreeOrBound::Bound(Unbounded)))
        {
            match tob {
                TreeOrBound::Tree(n) => {
                    assert!(untreated_subtree.is_none());
                    untreated_subtree = Some(n);
                }
                TreeOrBound::Bound(b) => {
                    subtree_range.0[0] = subtree_range.0[1];
                    subtree_range.0[1] = b;
                    if let Some(subtree) = untreated_subtree {
                        size += self.treap.count(subtree, &self.limits, &subtree_range);
                    }
                    untreated_subtree = None;
                }
            }
        }
        size + nodes_count
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
