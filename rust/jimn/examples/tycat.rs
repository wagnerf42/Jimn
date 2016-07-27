extern crate jimn;
use jimn::point::Point;
use jimn::tycat::{display, Displayable};

fn main() {
    let points = vec![
        Point::new(1.0, 0.0),
        Point::new(0.0, 1.0),
        Point::new(-1.0, 0.0),
        Point::new(0.0, -1.0)
    ];
    let references: Vec<&Displayable> = points.iter().map(|p| p as &Displayable).collect();
    display(&references);
}
