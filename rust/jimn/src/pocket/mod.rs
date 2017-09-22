//! `Pocket` class.
use ElementaryPath;

pub use self::pocket_builder::build_pockets;
mod pocket_builder;

/// `Polygon` equivalent, but also allowing arcs.
pub struct Pocket {
    _edge: Vec<ElementaryPath>,
}
