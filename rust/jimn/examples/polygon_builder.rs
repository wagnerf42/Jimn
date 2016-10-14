#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::polygon::builder::build_polygons;
use jimn::tycat::{Displayable, display};

fn main() {
    let polygons = build_polygons(
        vec![
        Segment::new(Point::new(0.0, 0.0), Point::new(2.0, 2.0)),
        Segment::new(Point::new(2.0, 2.0), Point::new(3.0, 1.0)),
        Segment::new(Point::new(3.0, 1.0), Point::new(0.0, 0.0)),
        ]
        );
    let iterator = polygons.iter();
    display!(iterator);
}
