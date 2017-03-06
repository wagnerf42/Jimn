#[macro_use]
extern crate jimn;
extern crate glob;
use glob::glob;
use jimn::segment::load_segments;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};

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

fn main() {
    for entry in glob("tests_bentley_ottmann/*bo").unwrap() {
        if let Ok(path) = entry {
            try_bentley_ottmann_on(path.to_str().unwrap());
        }
    }
}
