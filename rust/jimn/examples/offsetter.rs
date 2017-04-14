extern crate jimn;
use jimn::polygon::square;
use jimn::offsetter::offset_holed_polygon;
use jimn::holed_polygon::HoledPolygon;

fn main() {
    let s = square(0.0, 0.0, 5.0);
    let h = HoledPolygon::new(s, Vec::new());
    offset_holed_polygon(&h, 1.0);
    unimplemented!()
}
