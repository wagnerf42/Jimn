#[macro_use]
extern crate jimn;
use jimn::point::Point;
use jimn::polygon::Polygon;
//use jimn::tycat::{Displayable, display};
use jimn::tree::inclusion_tree::build_inclusion_tree;

fn main() {
    let outer_shape = Polygon::new(vec![
        Point::new(0.0, 0.0),
        Point::new(60.0, 0.0),
        Point::new(60.0, 60.0),
        Point::new(0.0, 60.0),
    ]);
    let triangle = Polygon::new(vec![
        Point::new(0.0, 25.0),
        Point::new(53.0, 25.0),
        Point::new(5.0, 0.0)
    ]);
    let square = Polygon::new(vec![
        Point::new(10.0, 10.0),
        Point::new(15.0, 10.0),
        Point::new(15.0, 15.0),
        Point::new(10.0, 15.0),
    ]);
    let slices = vec![
        (4.0, vec![outer_shape.clone()]),
        (3.0, vec![outer_shape.clone(), triangle, square.clone()]),
        (2.0, vec![outer_shape, square])
    ];
    build_inclusion_tree(slices);
}
