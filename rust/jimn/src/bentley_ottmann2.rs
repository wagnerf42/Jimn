//! new implementation for BO
use comparison::{KeyGenerator, ProvidesComparisonKey};

//events will need to use (point, end/start, keyatpoint) for cmp

/// Compute all intersections amongst given paths
/// and return the `KeyGenerator` which contains for each path all intersection points.
fn bentley_ottmann<'a, K: Copy + Ord, T: 'a + AsRef<ProvidesComparisonKey<Key = K>>>(
    paths: &[T],
) -> KeyGenerator<'a, K, T> {
    let mut generator = KeyGenerator::new(paths);
    unimplemented!()
}
