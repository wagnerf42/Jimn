extern crate jimn;
use jimn::stl::Stl;

fn main() {
    let model = Stl::new("../../test_files/cordoba.stl").expect("error loading");
    model.facets[0].intersect(0.96);
}
