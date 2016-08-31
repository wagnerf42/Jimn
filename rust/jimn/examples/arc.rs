#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::arc::Arc;
use jimn::tycat::{Displayable, display};

fn main() {
    let arc = Arc::new(
        3.0, Point::new(0.0, 3.0), Point::new(3.0, 0.0),
        Point::new(0.0, 0.0), true);
    display!(arc);
}
