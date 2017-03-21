#[macro_use]
extern crate jimn;
use jimn::polygon::{square, build_polygons};
use jimn::segment::Segment;
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::{display, colored_display};
use jimn::tile::hexagonal_tile;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::clipper::clip;

/// try a square tile on a triangle
fn main() {
    let polygon = square(0.0, 0.0, 3.0);
    let quadrant = polygon.get_quadrant();
    let mut rounder = PointsHash::new(6);
    for point in &polygon.points {
        rounder.hash_point(point);
    }

    let tile = hexagonal_tile(0.2, 0.2);
    let tiled = tile.tile(&quadrant, &mut rounder);
    display!(polygon, tiled);
    let segments = polygon.points
        .iter()
        .zip(polygon.points
                 .iter()
                 .cycle()
                 .skip(1))
        .map(|(&p1, &p2)| Segment::new(p1, p2))
        .collect();
    let mut clipped = clip(segments, tiled, &mut rounder);
    display!(clipped);
    let polygons = build_polygons(&mut clipped);
    display!(polygons);
    colored_display(&polygons).expect("display failed");
}
