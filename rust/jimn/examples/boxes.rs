extern crate jimn;
use jimn::point::Point;
use jimn::bounding_box::BoundingBox;

fn main() {
    let o = Point::new(0.0, 0.0);
    let o2 = Point::new(1.3, 2.5);
    let mut bbox = BoundingBox::empty_box(2);
    println!("box is {}", bbox);
    bbox.add_point(&o);
    bbox.add_point(&o2);
    println!("box is {}", bbox);
}
