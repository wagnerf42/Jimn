//! Provides `Treap` and `CountingTreap` structures for sweeping line algorithms.
use std::cmp::Ord;
use rand;

mod counters;
mod node;
mod rawtreap;
pub use self::counters::{Counter, EmptyCounter, Counting};
pub use self::node::Node;
pub use self::rawtreap::{Treap, CountingTreap};

/// Keys are always unique : typically, keys are suffixed by unique identifier.
/// For some operations we still need to work on the *real* keys without the added suffix.
pub trait UniqueKey: Ord {
    /// Return if two given keys are in fact identical.
    fn is_same_as(&self, other: &Self) -> bool;
}

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
    use super::*;
    use std::rc::Rc;
    use std::cell::RefCell;
    // we create in tests two nodes for each integer between 1 and TEST_LIMIT
    const TEST_LIMIT: u32 = 10;

    fn test_counting(treap: &CountingTreap<u32, u32, IdentityKeyComputer>) {
        for i in 1..TEST_LIMIT {
            let nodes_larger_than_i = treap.nodes().filter(|n| n.borrow().value >= i).count();
            assert!(treap.number_of_larger_nodes(&i) == nodes_larger_than_i);
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
            }
        }
    }
}
