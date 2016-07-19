extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::tycat::display;
use jimn::tycat::Displayable;

fn main() {
    let p1 = Point::new(0.0, 0.0);
    let p2 = Point::new(3.0, 4.0);
    let segment = Segment::new(&p1, &p2);
    let p3 = Point::new(1.0, 2.5);
    let things: Vec<&Displayable> = vec![
        &segment, &p2, &p3
    ];
    display(&things);
}
