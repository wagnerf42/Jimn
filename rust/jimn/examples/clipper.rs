#[macro_use]
extern crate jimn;
use jimn::{Point, Polygon};
use jimn::utils::coordinates_hash::PointsHash;
use jimn::quadrant::Shape;
use jimn::tile::hexagonal_tile;
use jimn::clipper::clip;
use jimn::polygon::build_polygons;

fn main() {
    let points = vec![
        Point::new(0.0, 0.0),
        Point::new(5.0, -1.0),
        Point::new(3.0, -3.0),
    ];

    let mut rounder = PointsHash::new(6);
    for point in &points {
        rounder.hash_point(point);
    }
    let triangle = Polygon::new(points);
    display!(triangle);
    let hexagons = hexagonal_tile(1.0, 1.0);
    let hexagons_segments = hexagons.tile(&triangle.get_quadrant(), &mut rounder);
    display!(triangle, unicolor!(&hexagons_segments));
    let clipping_segments: Vec<_> = triangle.segments().collect();
    let (mut inside, outside) = clip(&clipping_segments, &hexagons_segments, &mut rounder);
    display!(triangle, unicolor!(&inside));
    inside.extend(outside);
    display!(unicolor!(&inside));
    let polygons = build_polygons(&inside);
    display!(multicolor!(&polygons));
}
