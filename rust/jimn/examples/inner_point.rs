#[macro_use]
extern crate jimn;
use jimn::{Point, Polygon};

fn main() {
    let poly = Polygon::new(vec![
        Point::new(0.0, 2.0),
        Point::new(3.0, -1.0),
        Point::new(4.5, 1.5),
        Point::new(4.0, 3.0),
        Point::new(3.0, 2.0),
    ]);
    let inner_point = poly.inner_point();
    display!(poly, inner_point);
}
