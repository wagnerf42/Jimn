#[macro_use]
extern crate jimn;
use jimn::segment::load_segments;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};

fn try_bentley_ottmann_on(filename: &str) {
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
    try_bentley_ottmann_on("tests_bentley_ottmann/triangle_0.8.bo");
    try_bentley_ottmann_on("tests_bentley_ottmann/triangle_0.1.bo");
    //    let segments = vec![Segment::new(Point::new(0.0, -1.0), Point::new(5.0, 0.0)),
    //                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0))];
    //
    //    let segments = vec![Segment::new(Point::new(0.0, 0.0), Point::new(5.0, 0.0)),
    //                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0))];
    //
    //    let segments = vec![Segment::new(Point::new(0.0, 0.0), Point::new(5.0, 0.0)),
    //                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0)),
    //                        Segment::new(Point::new(0.0, -2.0), Point::new(4.0, 3.0))];
}
