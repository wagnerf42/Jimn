extern crate jimn;
use jimn::{Point, Polygon};
use jimn::offsetter::offset_holed_polygon;
use jimn::holed_polygon::HoledPolygon;
use jimn::utils::coordinates_hash::PointsHash;

fn main() {
    let mut rounder = PointsHash::new(6);
    let points: Vec<_> = vec![
        Point::new(0.0, 0.0),
        Point::new(5.0, 0.0),
        Point::new(3.0, 3.0),
        Point::new(2.0, 0.5),
        Point::new(1.0, 3.0),
    ].iter()
        .map(|p| rounder.hash_point(p))
        .collect();
    let p = Polygon::new(points);
    let h = HoledPolygon::new(p, Vec::new());
    offset_holed_polygon(&h, 0.5, &mut rounder);
    unimplemented!()
}
