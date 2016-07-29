#[macro_use]
extern crate jimn;
use jimn::stl::Stl;
use jimn::segment::Segment;

fn main() {
    let model = Stl::new("../../test_files/cordoba.stl").expect("error loading");
    let slice: Vec<Segment> = model.facets.iter().filter_map(|f| f.intersect(0.96)).collect();
    display!(slice);
}
