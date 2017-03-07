#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::segment::{Segment, load_segments};
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::utils::coordinates_hash::PointsHash;

fn main() {
    try_bentley_ottmann_on("overlap.bo");
}

fn try_bentley_ottmann_on(filename: &str) {
    println!("loading {}", filename);
    let segments = load_segments(filename).expect("error loading segments file");
    display!(segments);
    let mut rounder = PointsHash::new(6);
    for segment in &segments {
        rounder.hash_point(&segment.start);
        rounder.hash_point(&segment.end);
    }
    bentley_ottmann(&segments, &mut rounder);
}
