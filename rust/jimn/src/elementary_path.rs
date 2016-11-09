//! structure for storing elementary movements.
//! `ElementaryPath` handles both [Segment](segment/index.html)
//! and [Arc](arc/index.html).
use point::Point;
use segment::Segment;
use arc::Arc;
use utils::precision::is_almost;
use utils::Identifiable;
use tycat::Displayable;
use std::fmt::{Debug, Display};
use ordered_float::OrderedFloat;
use tree::treap::Positionable;

/// Elementary Path is either Segment or Arc
pub enum ElementaryPath {
    /// segment
    Segment(Segment),
    /// arc
    Arc(Arc)
}

impl ElementaryPath {
    /// When comparing two paths p1 and p2 in a sweeping line algorithm
    /// we need to figure out which one is above which other.
    /// Of course this depends on current x position.
    /// TODO: better documentation
    fn comparison_key(&self, current_x: f64) -> (OrderedFloat<f64>,
                                                 OrderedFloat<f64>,
                                                 OrderedFloat<f64>) {
        match *self {
            ElementaryPath::Segment(ref s) => s.comparison_key(current_x),
            ElementaryPath::Arc(ref a) => a.comparison_key(current_x)
        }
    }
}
