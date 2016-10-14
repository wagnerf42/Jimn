#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::tycat::{Displayable, display};
use std::collections::HashMap;

fn main() {
    let segments = vec![
        Segment::new(Point::new(0.0, 0.0), Point::new(1.1, 1.1)),
        Segment::new(Point::new(0.0, 1.0), Point::new(1.0, 0.0)),
    ];
    let point = Point::new(2.0, 3.0);
    let mut h = HashMap::new();
    h.insert(0, Point::new(1.0, 2.5));

    let i1 = segments.iter();
    let i2 = h.values();
    display!(i1, point, i2);
}
