#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::arc::Arc;
use jimn::pocket::Pocket;
use jimn::polygon::Polygon;
use jimn::pocket::builder::build_polygons;
use jimn::tycat::{Displayable, display};
use jimn::elementary_path::ElementaryPath;
use std::collections::HashMap;

fn main() {
    let arc = Box::new(Arc::new(
                3.0, Point::new(0.0, 3.0), Point::new(3.0, 0.0),
                Point::new(0.0, 0.0), true));
    let s1 = Box::new(Segment::new(
        Point::new(3.0, 0.0),
        Point::new(5.0, 5.0)));

    let s2 = Box::new(Segment::new(
        Point::new(5.0, 5.0),
        Point::new(0.0, 3.0)));

    let polygons = build_polygons(vec![arc, s1, s2]);
//    display!(polygons);

//    let pocket = Pocket::new(vec![arc, s1, s2]);
//    display!(pocket);
}
