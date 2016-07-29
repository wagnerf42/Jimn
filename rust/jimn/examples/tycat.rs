#[macro_use]
extern crate jimn;
use jimn::point::Point;

fn main() {
    let points = vec![
        Point::new(1.0, 0.0),
        Point::new(0.0, 1.0),
        Point::new(-1.0, 0.0),
        Point::new(0.0, -1.0)
    ];
    display!(points);
}
