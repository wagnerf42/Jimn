//! All helper functions not directly related to geometry go into submodules
//! here.
#[macro_use]
pub mod debug;
pub mod coordinates_hash;
pub mod precision;
pub mod float;

/// `Identifiable` allows equivalent to python's `id`
pub trait Identifiable {
    /// Returns address of object which serves as unique identifier.
    fn id(&self) -> usize {
        self as *const _ as *const () as usize
    }
}

/// We can now map on arrays (of small sizes)
/// see: https://llogiq.github.io/2016/04/28/arraymap.html
pub trait ArrayMap<X, Y, T> {
    /// Apply given function to all elements of array self to get a results array.
    fn map<F: FnMut(&X) -> Y>(&self, f: F) -> T;
}

impl<U, V> ArrayMap<U, V, [V; 2]> for [U; 2] {
    fn map<F: FnMut(&U) -> V>(&self, mut function: F) -> [V; 2] {
        [function(&self[0]), function(&self[1])]
    }
}

//TODO: macroize
impl<U, V> ArrayMap<U, V, [V; 3]> for [U; 3] {
    fn map<F: FnMut(&U) -> V>(&self, mut function: F) -> [V; 3] {
        [function(&self[0]), function(&self[1]), function(&self[2])]
    }
}
