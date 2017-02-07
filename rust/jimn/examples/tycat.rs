#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::quadrant::Quadrant;
use jimn::tycat::display;

fn main() {
    let points = vec![
        Point::new(2.0, 3.0),
        Point::new(1.0, 1.0)
    ];
    display!(points[0], points[1]);
}
