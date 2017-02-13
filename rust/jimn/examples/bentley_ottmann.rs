#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::display;
use jimn::bentley_ottmann::bentley_ottmann;

fn main() {
    let segments = vec![Segment::new(Point::new(0.0, 0.0), Point::new(5.0, 0.0)),
                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0))];
    display!(segments);
    let results = bentley_ottmann(&segments);
    display!(results);
}
