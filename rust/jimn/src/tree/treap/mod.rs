//! Provides `Treap` and `CountingTreap` structures for sweeping line algorithms.
use std::cmp::Ord;

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
