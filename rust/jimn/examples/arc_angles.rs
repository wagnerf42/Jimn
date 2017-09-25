#[macro_use]
extern crate jimn;
extern crate ordered_float;
use jimn::point::Point;
use jimn::{Arc, ElementaryPath, Segment};
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::{colored_display, display};
use ordered_float::NotNaN;
use jimn::bentley_ottmann::BentleyOttmannPath;
use std::f64::consts::PI;

fn angles(path: &ElementaryPath, point: &Point) -> (NotNaN<f64>, NotNaN<f64>) {
    let final_angle = path.start().angle_with(&path.end());
    match *path {
        ElementaryPath::Segment(ref s) => (final_angle, final_angle),
        ElementaryPath::Arc(ref a) => {
            let afgle = a.center.angle_with(&a.start);
            let angle = (NotNaN::new(PI).unwrap() * 2.0 - afgle) % (NotNaN::new(PI).unwrap() * 2.0);
            println!("{:?} {} {}", a, afgle, angle);
            (angle, final_angle)
        }
    }
}

fn main() {
    let o = Point::new(0.0, 0.0);
    let mut paths = vec![
        ElementaryPath::Segment(Segment::new(o, Point::new(1.0, -1.0))),
        ElementaryPath::Segment(Segment::new(o, Point::new(1.0, 1.0))),
        ElementaryPath::Arc(Arc::new(o, Point::new(1.0, 1.0), Point::new(0.0, 1.0), 1.0)),
        ElementaryPath::Arc(Arc::new(
            o,
            Point::new(1.0, -1.0),
            Point::new(0.0, -1.0),
            1.0,
        )),
        ElementaryPath::Segment(Segment::new(o, Point::new(1.0, 0.0))),
        ElementaryPath::Segment(Segment::new(o, Point::new(-1.0, 0.0))),
        ElementaryPath::Arc(Arc::new(
            o,
            Point::new(-1.0, 1.0),
            Point::new(0.0, 1.0),
            1.0,
        )),
    ];

    display!(paths);

    colored_display(paths.iter());
    paths.sort_by_key(|p| angles(p, &o));
    colored_display(paths.iter());


    println!("{}", o.angle_with(&Point::new(1.0, 0.0)));
    println!("{}", o.angle_with(&Point::new(0.0, 1.0)));
    println!("{}", o.angle_with(&Point::new(-1.0, 0.0)));
    println!("{}", o.angle_with(&Point::new(0.0, -1.0)));
}
