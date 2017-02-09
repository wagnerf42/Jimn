extern crate jimn;
use jimn::point::Point;
use jimn::quadrant::Quadrant;

fn main() {
    let o = Point::new(0.0, 0.0);
    let o2 = Point::new(1.3, 2.5);
    let mut quadrant = Quadrant::new(2);
    println!("quadrant is {:?}", quadrant);
    quadrant.add(&o);
    quadrant.add(&o2);
    println!("quadrant is {:?}", quadrant);
}
