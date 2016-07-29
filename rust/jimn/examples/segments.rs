#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;

fn main() {
    let segment1 = Segment::new(Point::new(0.0, 0.0), Point::new(3.0, 4.0));
    let segment2 = Segment::new(Point::new(0.0, -3.0), Point::new(2.0, 5.0));
    let intersection = segment1.intersection_with_segment(&segment2).unwrap();
    display!(segment1, segment2, intersection);
}
