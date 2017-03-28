#[macro_use]
extern crate jimn;
use jimn::polygon::square;
use jimn::holed_polygon::{HoledPolygon, build_holed_polygons};
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::{display, colored_display};

fn main() {
    println!("displaying two holed polygons");
    let holed_polygon1 = HoledPolygon::new(square(0.0, 0.0, 10.0),
                                           vec![square(1.0, 1.0, 3.0), square(5.0, 5.0, 2.0)]);
    let holed_polygon2 = HoledPolygon::new(square(12.0, 12.0, 3.0), Vec::new());
    display!(holed_polygon1, holed_polygon2);

    println!("auto-building holed polygons");
    let polygons = vec![square(0.0, 0.0, 10.0),
                        square(12.0, 0.0, 3.0),
                        square(2.0, 2.0, 5.0),
                        square(3.0, 3.0, 1.0),
                        square(7.5, 3.0, 2.0)];
    colored_display(polygons.iter());
    let holed_polygons = build_holed_polygons(polygons);
    colored_display(holed_polygons.iter());
}
