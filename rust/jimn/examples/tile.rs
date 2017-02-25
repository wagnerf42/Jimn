#[macro_use]
extern crate jimn;
use jimn::quadrant::{Quadrant, Shape};
use jimn::point::Point;
use jimn::segment::{Segment, save_segments};
use jimn::tycat::display;
use jimn::tile::rectangular_tile;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::utils::ArrayMap;

/// try a square tile on a triangle
fn main() {
    let mut rounder = PointsHash::new(6);
    let points = [Point::new(2.0, 2.0), Point::new(7.0, 3.0), Point::new(5.0, 5.0)]
        .map(|p| rounder.hash_point(p));

    let triangle = vec![Segment::new(points[0], points[1]),
                        Segment::new(points[0], points[2]),
                        Segment::new(points[1], points[2])];
    display!(triangle);
    let mut quadrant = Quadrant::new(2);
    for point in &points {
        quadrant.add(point);
    }
    //let square_tile = rectangular_tile(0.8, 0.8);
    let square_tile = rectangular_tile(0.8, 0.8); // we crash here
    let tiled_triangle = square_tile.tile(&quadrant, &mut rounder);
    display!(triangle, tiled_triangle);

    let mut all = Vec::new();
    all.extend(triangle);
    all.extend(tiled_triangle);
    save_segments("triangle_0.8.bo", &all).expect("failed writing");
    bentley_ottmann(&all, &mut rounder);
}
