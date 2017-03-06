#[macro_use]
extern crate jimn;
extern crate rand;
use jimn::point::Point;
use jimn::segment::{Segment, save_segments};
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::utils::coordinates_hash::PointsHash;

fn random_point(rounder: &mut PointsHash) -> Point {
    rounder.hash_point(&Point::new(rand::random::<f64>(), rand::random::<f64>()))
}

fn main() {
    let mut rounder = PointsHash::new(6);
    let segments: Vec<Segment> = (0..200)
        .map(|_| Segment::new(random_point(&mut rounder), random_point(&mut rounder)))
        .collect();
    display!(segments);
    save_segments("random_200.bo", &segments).expect("failed saving segments");
    bentley_ottmann(&segments, &mut rounder);
}
