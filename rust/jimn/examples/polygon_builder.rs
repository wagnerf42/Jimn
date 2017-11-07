#[macro_use]
extern crate jimn;
use jimn::segment::load_segments;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::polygon::build_polygons;
use jimn::overlap::cut_overlaps;

fn main() {
    let segments = load_segments("tests_bentley_ottmann/triangle_h_1.0.bo")
        .expect("error loading segments file");
    display!(unicolor!(&segments));
    let mut rounder = PointsHash::new(6);
    for segment in &segments {
        rounder.hash_point(&segment.start);
        rounder.hash_point(&segment.end);
    }
    let no_overlap_segments = cut_overlaps(&segments);
    let small_segments = bentley_ottmann(&no_overlap_segments, &mut rounder);
    let polygons = build_polygons(&small_segments);
    display!(multicolor!(&polygons));
}
