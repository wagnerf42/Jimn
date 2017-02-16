#[macro_use]
extern crate jimn;
extern crate ordered_float;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::display;

fn main() {
    let o = Point::new(0.0, 0.0);
    let points = [Point::new(1.0, 0.0),
                  Point::new(1.0, 0.05),
                  Point::new(1.0, 1.0),
                  Point::new(0.0, 1.0),
                  Point::new(-1.0, 1.0),
                  Point::new(-1.0, 0.05),
                  Point::new(-1.0, 0.0)];

    let segments: Vec<Segment> = points.iter().map(|&p| Segment::new(o, p)).collect();

    for segment in &segments {
        let angle = segment.sweeping_angle();
        println!("angle de {:?} : {}", segment, angle);
        display!(segments, segment);
    }
}
