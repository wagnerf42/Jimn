#[macro_use]
extern crate jimn;
use std::f64::consts::FRAC_PI_4;
use jimn::{Arc, ElementaryPath, Point, Segment};
use jimn::bentley_ottmann::{BentleyOttmannPath, YCoordinate};

fn main() {
    let o = Point::new(0.0, 0.0);
    let points = [
        Point::new(1.0, 0.0),
        Point::new(1.0, 0.05),
        Point::new(1.0, 1.0),
        Point::new(0.0, 1.0),
        Point::new(-1.0, 1.0),
        Point::new(-1.0, 0.05),
        Point::new(-1.0, 0.0),
        Point::new(-1.0, -1.0),
    ];

    let segments: Vec<Segment> = points.iter().map(|&p| Segment::new(o, p)).collect();

    for segment in &segments {
        let angle = segment.sweeping_angle();
        println!("angle de {:?} : {}", segment, angle);
        display!(unicolor!(&segments), segment);
    }

    let paths = vec![
        ElementaryPath::Segment(Segment::new(Point::new(-3.0, 3.0), Point::new(3.0, -3.0))),
        ElementaryPath::Segment(Segment::new(Point::new(0.0, 2.0), Point::new(0.0, -2.0))),
        ElementaryPath::Arc(Arc::new(
            Point::new(-1.0 + FRAC_PI_4.cos(), FRAC_PI_4.sin()),
            Point::new(-1.0 + FRAC_PI_4.cos(), -FRAC_PI_4.sin()),
            Point::new(-1.0, 0.0),
            1.0,
        )),
        ElementaryPath::Arc(Arc::new(
            Point::new(1.0 + (3.0 * FRAC_PI_4).cos(), (3.0 * FRAC_PI_4).sin()),
            Point::new(1.0 + (3.0 * FRAC_PI_4).cos(), -(3.0 * FRAC_PI_4).sin()),
            Point::new(1.0, 0.0),
            1.0,
        )),
    ];
    println!("**********************************");
    println!("**after arriving******************");
    println!("**********************************");
    for path in &paths {
        println!("key {:?}", path.compute_key(YCoordinate(0.0)));
        display!(unicolor!(&paths), path);
    }

    // last test
    let arc = Arc::new(
        Point::new(5.0, 0.5),
        Point::new(4.583974852831078, -0.27735009811261446),
        Point::new(5.0, 0.0),
        0.5,
    );
    let path = ElementaryPath::Arc(arc);
    let y = -0.27735009811261446;
    let intersection = arc.horizontal_line_intersection(y);
    println!("key {:?}", path.compute_key(YCoordinate(y)));
    display!(path, intersection);
}
