#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::polygon::Polygon;

fn main() {
    let points = vec![Point::new(2.0, 3.0), Point::new(1.0, 1.0)];
    let segment = Segment::new(points[0], points[1]);
    let poly = Polygon::new(vec![
        Point::new(0.0, 0.0),
        Point::new(1.0, 3.0),
        Point::new(3.0, -1.0),
    ]);
    display!(points[0], points[1], segment, poly);
}
