#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::polygon::Polygon;
use jimn::tycat::{Displayable, display};

fn main() {
    let triangle = Polygon::new(vec![
        Point::new(0.0, 2.5),
        Point::new(1.3, 2.5),
        Point::new(0.5, 4.1)
    ]);
    display!(triangle);
}
