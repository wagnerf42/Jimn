extern crate jimn;
use jimn::tree::treap::Treap;
use jimn::point::Point;
use jimn::segment::Segment;

fn main() {
    let mut tree = Treap::new();
    tree.set_position(1.0);
    let content = vec![
        Segment::new(Point::new(0.0, 0.0), Point::new(3.0, 0.0)),
        Segment::new(Point::new(0.0, 0.0), Point::new(2.0, 1.0)),
        Segment::new(Point::new(0.5, 1.0), Point::new(3.0, 3.0)),
    ];
    tree.populate(content);
    tree.tycat();
}
