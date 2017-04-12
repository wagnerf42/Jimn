//! This module holds the `ElementaryPath` class.
use {Arc, Segment};

/// Elementary path (used for building larger paths)
/// can be either:
/// - `Arc`
/// - `Segment`
/// - vertical segment
pub enum ElementaryPath {
    /// `Arc` path
    Arc(Arc),
    /// `Segment` path
    Segment(Segment),
}
