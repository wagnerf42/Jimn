#[macro_use]
extern crate jimn;
use jimn::segment::load_segments;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::bentley_ottmann::{bentley_ottmann, cut_paths};
use jimn::tycat::{colored_display, display};
use jimn::quadrant::{Quadrant, Shape};
use jimn::polygon::build_polygons;

fn main() {
    let segments = load_segments("tests_bentley_ottmann/triangle_h_1.0.bo")
        .expect("error loading segments file");
    display!(segments);
    let mut rounder = PointsHash::new(6);
    for segment in &segments {
        rounder.hash_point(&segment.start);
        rounder.hash_point(&segment.end);
    }
    let intersections = bentley_ottmann(&segments);
    let small_segments = cut_paths(&segments, &intersections, &mut rounder);
    let polygons = build_polygons(&small_segments);
    colored_display(&polygons).expect("display failed");
}
