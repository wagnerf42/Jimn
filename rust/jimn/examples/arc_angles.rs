#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::{Arc, ElementaryPath, Segment};
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::{colored_display, display};
use std::f64::consts::{FRAC_PI_2, PI};

fn angles(path: &ElementaryPath, point: &Point) -> (f64, f64) {
    let final_angle = path.start().angle_with(&path.end());
    match *path {
        ElementaryPath::Segment(_) => (final_angle, final_angle),
        ElementaryPath::Arc(ref a) => {
            let mut tangent_angle = a.center.angle_with(&point) + FRAC_PI_2;
            if ((final_angle - tangent_angle) % (PI * 2.0)).abs() > PI {
                tangent_angle -= PI;
            }
            (tangent_angle % (PI * 2.0), final_angle)
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

    paths.sort_by(|p1, p2| {
        angles(p1, &o).partial_cmp(&angles(p2, &o)).unwrap()
    });
    colored_display(paths.iter()).expect("display failed");
    for path in &paths {
        println!("angle is {:?}", angles(path, &o));
    }
}
