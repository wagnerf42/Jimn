extern crate jimn;
use jimn::stl::Stl;
use jimn::segment::Segment;
use jimn::tycat::{display, Displayable};

fn main() {
    let model = Stl::new("../../test_files/cordoba.stl").expect("error loading");
    let slice: Vec<Segment> = model.facets.iter().filter_map(|f| f.intersect(0.96)).collect();
    let refs: Vec<&Displayable> = slice.iter().map(|d| d as &Displayable).collect();
    display(&refs);
}
