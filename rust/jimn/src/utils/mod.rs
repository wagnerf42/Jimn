//! All helper functions not directly related to geometry go into submodules
//! here.
//#![macro_use]
//#[macro_use]
//pub mod debug;
//pub mod coordinates_hash;
pub mod precision;

/// `Identifiable` allows equivalent to python's `id`
pub trait Identifiable {
    /// Returns address of object which serves as unique identifier.
    fn id(&self) -> usize {
        self as *const _ as *const() as usize
    }
}
