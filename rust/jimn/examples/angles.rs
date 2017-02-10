extern crate jimn;
use jimn::point::Point;

fn main() {
    let o = Point::new(0.0, 0.0);
    let points =
        [Point::new(1.0, 0.0), Point::new(0.0, 1.0), Point::new(-1.0, 0.0), Point::new(0.0, -1.0)];

    for point in &points {
        println!("angle avec {:?} : {}", point, o.angle_with(point))
    }
}
