//! This module holds the `ElementaryPath` class.
use std::f64::consts::FRAC_PI_2;
use std::collections::HashSet;
use ordered_float::NotNaN;
use {Arc, Point, Segment};
use quadrant::{Quadrant, Shape};
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::Cuttable;

/// Elementary path (used for building larger paths)
/// can be either:
/// - `Arc`
/// - `Segment`
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub enum ElementaryPath {
    /// `Arc` path
    Arc(Arc),
    /// `Segment` path
    Segment(Segment),
}

impl Cuttable for ElementaryPath {
    fn cut(&self, points: &HashSet<Point>) -> Vec<Self> {
        match *self {
            ElementaryPath::Arc(a) => a.cut(points).into_iter().map(ElementaryPath::Arc).collect(),
            ElementaryPath::Segment(s) => s.cut(points)
                .into_iter()
                .map(ElementaryPath::Segment)
                .collect(),
        }
    }
}

impl AsRef<ElementaryPath> for ElementaryPath {
    fn as_ref(&self) -> &ElementaryPath {
        self
    }
}

impl Shape for ElementaryPath {
    fn get_quadrant(&self) -> Quadrant {
        match *self {
            ElementaryPath::Arc(ref a) => a.get_quadrant(),
            ElementaryPath::Segment(ref s) => s.get_quadrant(),
        }
    }

    fn svg_string(&self) -> String {
        match *self {
            ElementaryPath::Arc(ref a) => a.svg_string(),
            ElementaryPath::Segment(ref s) => s.svg_string(),
        }
    }
}

impl ElementaryPath {
    /// Create an `ElementaryPath` segment parallel to given one.
    /// at given distance and we can be on right or left side.
    pub fn parallel_segment(
        segment: &Segment,
        distance: NotNaN<f64>,
        right_side: bool,
        rounder: &mut PointsHash,
    ) -> ElementaryPath {
        let direction = if right_side { 1.0 } else { -1.0 };
        let angle = segment.start.angle_with(&segment.end) + FRAC_PI_2 * direction;
        let displacement = Point::new(distance * (angle).cos(), distance * (angle).sin());
        let start = segment.start + displacement;
        let end = segment.end + displacement;
        ElementaryPath::Segment(Segment::new(
            rounder.hash_point(&start),
            rounder.hash_point(&end),
        ))
    }

    /// Return ref on starting point.
    pub fn start(&self) -> &Point {
        match *self {
            ElementaryPath::Segment(ref s) => &s.start,
            ElementaryPath::Arc(ref a) => &a.start,
        }
    }

    /// Return ref on ending point.
    pub fn end(&self) -> &Point {
        match *self {
            ElementaryPath::Segment(ref s) => &s.end,
            ElementaryPath::Arc(ref a) => &a.end,
        }
    }
}
