extern crate jimn;
use jimn::polygon::Polygon;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::tree::inclusion_tree::polygon_segment::PolygonSegment;

fn main() {
    let p = Polygon::new(vec![
        Point::new(0.0, 0.0),
        Point::new(6.0, 0.0),
        Point::new(3.0, 3.0),
    ]);
    let s = PolygonSegment::new(&p, 0, 0.0);
    println!("{:?}", s);
}
