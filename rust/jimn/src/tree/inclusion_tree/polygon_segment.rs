//! `Segment` part of a `Polygon`.
//! Used for classifying polygons when building `InclusionTree`.

use point::Point;
use segment::Segment;
use polygon::Polygon;
use elementary_path::ElementaryPath;
use bounding_box::BoundingBox;
use tycat::{Displayable, Displayer};
use utils::Identifiable;

/// A `PolygonSegment` contains a segment while remembering
/// to which `Polygon` it belongs and at which height it is.
#[derive(Debug)]
pub struct PolygonSegment<'a> {
    polygon: &'a Polygon,
    segment: Segment,
    height: f64
}

impl<'a> PolygonSegment<'a> {
    /// Creates a new PolygonSegment starting at polygon point of
    /// given index.
    pub fn new(polygon: &Polygon, point_index: usize, height: f64)
        -> PolygonSegment {
            PolygonSegment {
                polygon: polygon,
                segment: polygon.segment(point_index),
                height: height
            }
    }
}

impl<'a> Displayable for PolygonSegment<'a> {
    fn get_bounding_box(&self) -> BoundingBox {
        self.segment.get_bounding_box()
    }
    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        self.segment.save_svg_content(displayer, color)
    }
}

impl<'a> Identifiable for PolygonSegment<'a> {}
impl<'a> ElementaryPath for PolygonSegment<'a> {
    fn points(&self) -> (Point, Point) {
        self.segment.points()
    }
}
