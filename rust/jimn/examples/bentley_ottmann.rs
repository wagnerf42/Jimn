#[macro_use]
extern crate jimn;
use std::env::args;
use jimn::segment::load_segments;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::overlap::remove_overlaps;

fn try_bentley_ottmann_on<T: AsRef<str>>(filename: &T) {
    println!("loading {}", filename.as_ref());
    let segments = load_segments(filename.as_ref()).expect("error loading segments file");
    display!(segments);
    let mut rounder = PointsHash::new(6);
    for segment in &segments {
        rounder.hash_point(&segment.start);
        rounder.hash_point(&segment.end);
    }
    let no_overlap_segments = remove_overlaps(&segments);
    let small_segments = bentley_ottmann(&no_overlap_segments, &mut rounder);
    display!(small_segments);
    println!("we now have {} segments", small_segments.len());
}

fn main() {
    for path in args().skip(1) {
        try_bentley_ottmann_on(&path);
    }
}
