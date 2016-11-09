//! `Segment` part of a `Polygon`.
//! Used for classifying polygons when building `InclusionTree`.
use std::fmt;
use ordered_float::OrderedFloat;

use point::Point;
use segment::Segment;
use polygon::Polygon;
use bounding_box::BoundingBox;
use tycat::{Displayable, Displayer};
use utils::Identifiable;
//use tree::treap::Positionable;
//
///// A `PolygonSegment` contains a segment while remembering
///// to which `Polygon` it belongs and at which height it is.
//#[derive(Debug)]
//pub struct PolygonSegment<'a> {
//    polygon: &'a Polygon,
//    segment: Segment,
//    height: f64
//}
//
//impl<'a> PolygonSegment<'a> {
//    /// Creates a new PolygonSegment starting at polygon point of
//    /// given index.
//    pub fn new(polygon: &Polygon, point_index: usize, height: f64)
//        -> PolygonSegment {
//            PolygonSegment {
//                polygon: polygon,
//                segment: polygon.segment(point_index),
//                height: height
//            }
//    }
//}
//
//impl<'a> Displayable for PolygonSegment<'a> {
//    fn get_bounding_box(&self) -> BoundingBox {
//        self.segment.get_bounding_box()
//    }
//    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
//        self.segment.save_svg_content(displayer, color)
//    }
//}
//
//impl<'a> Identifiable for PolygonSegment<'a> {}
//impl<'a> fmt::Display for PolygonSegment<'a> {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        write!(f, "{}", self.segment)
//    }
//}
//
//impl<'a> Positionable for PolygonSegment<'a> {
//    fn comparison_key(&self, current_x: f64)
//        -> (OrderedFloat<f64>, OrderedFloat<f64>, OrderedFloat<f64>) {
//        self.segment.comparison_key(current_x)
//    }
//}
