#[macro_use]
extern crate jimn;
use jimn::segment::Segment;
use jimn::point::Point;
use jimn::overlap::remove_overlaps;

fn main() {
    let segments = vec![
        Segment::new(Point::new(0.0, 0.0), Point::new(3.0, 3.0)),
        Segment::new(Point::new(2.0, 2.0), Point::new(5.0, 5.0)),
        Segment::new(Point::new(5.0, 5.0), Point::new(7.0, 7.0)),
        Segment::new(Point::new(0.0, 0.0), Point::new(4.0, 2.0)),
        Segment::new(Point::new(2.0, 1.0), Point::new(6.0, 3.0)),
    ];
    display!(multicolor!(&segments));
    let remaining_parts = remove_overlaps(&segments);
    println!("remains: {:?}", remaining_parts);
    display!(multicolor!(&remaining_parts));
}
