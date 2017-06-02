//! Provides `Treap` and `CountingTreap` structures for sweeping line algorithms.
use std::cmp::Ord;

mod counters;
mod node;
mod rawtreap;
pub use self::counters::{Counter, EmptyCounter, Counting};
pub use self::node::Node;
pub use self::rawtreap::{Treap, CountingTreap};

/// Objets inside a treap must be comparable by someone.
/// We use a `KeyComputer` to return a comparison key for an object.
pub trait KeyComputer<T, U: Ord> {
    ///Returns a comparison key of type *U* for comparing objects of type *T*.
    fn compute_key(&self, object: &T) -> U;
}

/// We give a default comparer for cases where comparison keys are directly the
/// objects compared.
pub struct IdentityKeyComputer();
impl<T: Clone + Ord> KeyComputer<T, T> for IdentityKeyComputer {
    fn compute_key(&self, object: &T) -> T {
        object.clone()
    }
}

#[cfg(test)]
mod tests {
    use rand;
    use super::*;
    use std::rc::Rc;
    use std::cell::RefCell;
    use test::Bencher;
    // we create in tests two nodes for each integer between 1 and TEST_LIMIT
    const TEST_LIMIT: u32 = 10;

    /// test `number_of_larger_nodes`
    fn test_counting(treap: &CountingTreap<u32, u32, IdentityKeyComputer>) {
        for i in 1..TEST_LIMIT {
            let nodes_larger_than_i = treap.nodes().filter(|n| n.borrow().value >= i).count();
            assert!(treap.number_of_larger_nodes(&i) == nodes_larger_than_i);
        }
    }

    /// test `nearest_nodes`
    fn test_nearest(treap: &CountingTreap<u32, u32, IdentityKeyComputer>) {
        for i in 1..TEST_LIMIT {
            // manually count
            let slightly_larger_key = treap
                .nodes()
                .map(|n| n.borrow().value)
                .filter(|v| *v > i)
                .min();
            let number_of_appearances = if slightly_larger_key.is_some() {
                treap
                    .nodes()
                    .map(|n| n.borrow().value)
                    .filter(|v| *v == slightly_larger_key.unwrap())
                    .count()
            } else {
                0
            };

            let count = treap.nearest_nodes(i, 1).count();
            assert_eq!(count, number_of_appearances);
        }
    }

    #[test]
    /// Let's test if all neighbour related functions are resistant to duplicated keys.
    fn test_duplicated_keys() {
        let treap = CountingTreap::new(Rc::new(RefCell::new(IdentityKeyComputer())));
        for i in 1..TEST_LIMIT {
            treap.add(i);
            treap.add(i);
        }
        while !treap.empty() {
            let target_value = rand::random::<u32>() % TEST_LIMIT;
            if let Some(node) = treap.find_node(target_value) {
                node.remove();
                test_counting(&treap);
                test_nearest(&treap);
            }
        }
    }

    #[bench]
    fn treap_1000_integers_insertions(b: &mut Bencher) {
        b.iter(|| {
                   let mut treap = Treap::new(Rc::new(RefCell::new(IdentityKeyComputer())));
                   treap.populate(1..1000);
               });
    }

    #[bench]
    fn ctreap_1000_integers_insertions(b: &mut Bencher) {
        b.iter(|| {
                   let mut treap = CountingTreap::new(Rc::new(RefCell::new(IdentityKeyComputer())));
                   treap.populate(1..1000);
               });
    }

}
