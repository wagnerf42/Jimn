#[macro_use]
extern crate jimn;
extern crate rand;
use jimn::point::Point;
use jimn::segment::{Segment, save_segments, load_segments};
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::utils::coordinates_hash::PointsHash;

fn random_point(rounder: &mut PointsHash) -> Point {
    rounder.hash_point(&Point::new(rand::random::<f64>(), rand::random::<f64>()))
}

fn main() {

    //    let segments = load_segments("test.bo").expect("error loading segments file");
    //    display!(segments);
    //    let mut rounder = PointsHash::new(6);
    //    for segment in &segments {
    //        rounder.hash_point(&segment.start);
    //        rounder.hash_point(&segment.end);
    //    }
    //    bentley_ottmann(&segments, &mut rounder);
    for i in 0..3 {
        println!("{}", i);
        let mut rounder = PointsHash::new(6);
        let segments: Vec<Segment> = (0..100)
            .map(|_| Segment::new(random_point(&mut rounder), random_point(&mut rounder)))
            .collect();
        //display!(segments);
        //save_segments("test.bo", &segments);
        bentley_ottmann(&segments, &mut rounder);
    }
}
