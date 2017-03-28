#[macro_use]
extern crate jimn;
use std::env::args;
use jimn::point::Point;
use jimn::segment::load_segments;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::bentley_ottmann::{bentley_ottmann, cut_segments};
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};

fn try_bentley_ottmann_on<T: AsRef<str>>(filename: &T) {
    println!("loading {}", filename.as_ref());
    let segments = load_segments(filename.as_ref()).expect("error loading segments file");
    display!(segments);
    let mut rounder = PointsHash::new(6);
    for segment in &segments {
        rounder.hash_point(&segment.start);
        rounder.hash_point(&segment.end);
    }
    let intersections = bentley_ottmann(&segments, &mut rounder);
    let points: Vec<&Point> = intersections.values().flat_map(|points| points.iter()).collect();
    display!(segments, points);
    let small_segments = cut_segments(&segments, &intersections);
    display!(small_segments);
}

fn main() {
    for path in args().skip(1) {
        try_bentley_ottmann_on(&path);
    }
}
