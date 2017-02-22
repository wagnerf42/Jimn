#[macro_use]
extern crate jimn;
use jimn::quadrant::{Quadrant, Shape};
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::tycat::display;
use jimn::tile::rectangular_tile;

/// try a square tile on a triangle
fn main() {
    let points = [Point::new(2.0, 2.0), Point::new(7.0, 3.0), Point::new(5.0, 5.0)];
    let triangle = vec![Segment::new(points[0], points[1]),
                        Segment::new(points[0], points[2]),
                        Segment::new(points[1], points[2])];
    display!(triangle);
    let mut quadrant = Quadrant::new(2);
    for point in &points {
        quadrant.add(point);
    }
    let square_tile = rectangular_tile(2.0, 1.0);
    let tiled_triangle = square_tile.tile(&quadrant);
    display!(triangle, tiled_triangle);
}
