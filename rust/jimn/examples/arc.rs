#[macro_use]
extern crate jimn;
use std::f64::consts::PI;
use jimn::point::Point;
use jimn::Arc;

fn main() {
    let origin = Point::new(0.0, 0.0);
    let arcs = vec![
        Arc::new(Point::new(1.0, 0.0), Point::new(0.0, -1.0), origin, 1.0),
        Arc::new(
            Point::new(2.0, 0.0),
            Point::new((PI - 0.1).cos() * 2.0, (-(PI - 0.1).sin())),
            origin,
            2.0,
        ),
        Arc::new(Point::new(3.0, 0.0), Point::new(0.0, 3.0), origin, 3.0),
        Arc::new(Point::new(0.0, 3.1), Point::new(3.1, 0.0), origin, 3.1),
        Arc::new(
            Point::new((PI * 1.2).cos(), (PI * 1.2).sin()) * 4.0,
            Point::new(0.0, 4.0),
            origin,
            4.0,
        ),
    ];
    println!("red: quarter circle {:?}", arcs[0]);
    println!("green: almost half circle {:?}", arcs[1]);
    println!("blue: quarter circle one way {:?}", arcs[2]);
    println!("purple: quarter circle other way {:?}", arcs[3]);
    println!("orange: other quarter circle {:?}", arcs[4]);
    display!(multicolor!(&arcs));
}
