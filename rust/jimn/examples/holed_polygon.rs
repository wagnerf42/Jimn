#[macro_use]
extern crate jimn;
use jimn::polygon::square;
use jimn::holed_polygon::HoledPolygon;
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::display;

fn main() {
    let holed_polygon1 = HoledPolygon::new(square(0.0, 0.0, 10.0),
                                           vec![square(1.0, 1.0, 3.0), square(5.0, 5.0, 2.0)]);
    let holed_polygon2 = HoledPolygon::new(square(12.0, 12.0, 3.0), Vec::new());
    display!(holed_polygon1, holed_polygon2);
}
