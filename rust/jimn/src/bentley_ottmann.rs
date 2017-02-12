//! Bentley Ottmann intersection algorithm.
use std::collections::{HashMap, HashSet};
use point::Point;
use segment::Segment;

/// The `Cutter` structure holds all for bentley ottmann's execution.
struct Cutter {
    /// Results: we associate to each segment a set of intersections.
    intersections: HashMap<usize, HashSet<Point>>,
}

/// Computes all intersections amongst given segments
/// and return vector of obtained elementary segments.
pub fn bentley_ottmann(segments: &Vec<Segment>) -> Vec<Segment> {
    panic!("TODO");
}
