#[macro_use]
extern crate jimn;
use std::f64::consts::FRAC_PI_4;
use jimn::{Segment, Point, ElementaryPath, Arc};
use jimn::quadrant::{Quadrant, Shape};
use jimn::tycat::display;
use jimn::bentley_ottmann::BentleyOttmannPath;

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
        display!(segments, segment);
    }

    let paths = vec![
        ElementaryPath::Segment(Segment::new(Point::new(-3.0, 3.0), Point::new(3.0, -3.0))),
        ElementaryPath::Segment(Segment::new(Point::new(0.0, 2.0), Point::new(0.0, -2.0))),
        ElementaryPath::Arc(Arc::new(Point::new(-1.0+FRAC_PI_4.cos(), FRAC_PI_4.sin()), Point::new(-1.0+FRAC_PI_4.cos(), -FRAC_PI_4.sin()), Point::new(-1.0, 0.0), 1.0)),
        ElementaryPath::Arc(Arc::new(Point::new(1.0+(3.0*FRAC_PI_4).cos(), (3.0*FRAC_PI_4).sin()), Point::new(1.0+(3.0*FRAC_PI_4).cos(), -(3.0*FRAC_PI_4).sin()), Point::new(1.0, 0.0), 1.0)),
    ];
    println!("**********************************");
    println!("**before arriving*****************");
    println!("**********************************");
    for path in &paths {
        println!("key {:?}", path.compute_key(&Point::new(4.0, 0.0), &None));
        display!(paths, path);
    }
    println!("**********************************");
    println!("**after arriving******************");
    println!("**********************************");
    for path in &paths {
        println!("key {:?}", path.compute_key(&o, &None));
        display!(paths, path);
    }
}
