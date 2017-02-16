extern crate jimn;
use jimn::point::Point;
use jimn::segment::Segment;
use jimn::bentley_ottmann::bentley_ottmann;

fn main() {
    println!("base case : two intersecting segments");
    let segments = vec![Segment::new(Point::new(0.0, -1.0), Point::new(5.0, 0.0)),
                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0))];
    bentley_ottmann(&segments);

    println!("base case 2 : intersecting horizontal lines");
    let segments = vec![Segment::new(Point::new(0.0, 0.0), Point::new(5.0, 0.0)),
                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0))];
    bentley_ottmann(&segments);

    println!("base case 3 : segments intersecting twice");
    let segments = vec![Segment::new(Point::new(0.0, 0.0), Point::new(5.0, 0.0)),
                        Segment::new(Point::new(4.0, -2.0), Point::new(1.0, 2.0)),
                        Segment::new(Point::new(0.0, -2.0), Point::new(4.0, 3.0))];
    bentley_ottmann(&segments);

}
