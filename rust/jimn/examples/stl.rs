#[macro_use]
extern crate jimn;
use jimn::stl::Stl;
use jimn::segment::Segment;
use jimn::tycat::{Displayable, display};
use jimn::utils::coordinates_hash::PointsHash;

fn main() {
    let model = Stl::new("../../test_files/cordoba.stl")
        .expect("error loading");
    let mut points_hash = PointsHash::new(2, 6);
    let slice: Vec<Segment> = model.facets.iter()
        .filter_map(|f| f.intersect(0.96, &mut points_hash)).collect();
    display!(slice);
}
