extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::tycat::display;
use jimn::tycat::Displayable;

fn main() {
    let p1 = Point::new(0.0, 0.0);
    let p2 = Point::new(3.0, 4.0);
    let segment1 = Segment::new(p1, p2);
    let segment2 = Segment::new(
        Point::new(0.0, -3.0),
        Point::new(2.0, 5.0));
    let intersection = segment1.intersection_with_segment(&segment2).unwrap();
    let things: Vec<&Displayable> = vec![
        &segment1, &segment2, &intersection
    ];
    display(&things);
}
