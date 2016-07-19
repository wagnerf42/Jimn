extern crate jimn;
use jimn::point::Point;
use jimn::tycat::display;

fn main() {
    let points = vec![
        Point::new(1.0, 0.0),
        Point::new(0.0, 1.0),
        Point::new(-1.0, 0.0),
        Point::new(0.0, -1.0)
    ];
    display(&points);
    let points2 = vec![
        Point::new(1.0, 0.0),
        Point::new(2.0, 0.0),
        Point::new(5.0, 2.0)
    ];
    display(&points2);
}
