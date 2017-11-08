//! Provide `TaggedPath` type for storing final paths. This is not compressed in any way.
use {ElementaryPath, Point};
use quadrant::{Quadrant, Shape};

/// Path chunk in final path.
#[derive(Clone)]
pub enum TaggedPath {
    /// outer perimeter (displayed in red)
    Shell(ElementaryPath),
    /// inner filling (displayed in green)
    Fill(ElementaryPath),
    /// do not extrude (displayed in blue)
    Move(ElementaryPath),
    /// move one layer upwards (not displayed)
    Up,
}

impl TaggedPath {
    /// Return path's length (0 for upward paths).
    pub fn length(&self) -> f64 {
        match *self {
            TaggedPath::Shell(ref p) | TaggedPath::Fill(ref p) | TaggedPath::Move(ref p) => {
                p.length()
            }
            TaggedPath::Up => 0.0,
        }
    }
    /// Return references on our two endpoints in array.
    pub fn ends(&self) -> [&Point; 2] {
        match *self {
            TaggedPath::Shell(ref p) | TaggedPath::Fill(ref p) | TaggedPath::Move(ref p) => {
                [p.start(), p.end()]
            }
            TaggedPath::Up => panic!("no endpoints for upward paths"),
        }
    }
}

impl Shape for TaggedPath {
    fn get_quadrant(&self) -> Quadrant {
        match *self {
            TaggedPath::Up => Quadrant::new(2),
            TaggedPath::Shell(ref p) | TaggedPath::Fill(ref p) | TaggedPath::Move(ref p) => {
                p.get_quadrant()
            }
        }
    }
    fn svg_string(&self) -> String {
        match *self {
            TaggedPath::Up => String::new(),
            TaggedPath::Shell(ref p) => {
                String::from("<g stroke=\"red\">") + &p.svg_string() + &String::from("</g>")
            }
            TaggedPath::Fill(ref p) => {
                String::from("<g stroke=\"green\">") + &p.svg_string() + &String::from("</g>")
            }
            TaggedPath::Move(ref p) => {
                String::from("<g stroke=\"blue\">") + &p.svg_string() + &String::from("</g>")
            }
        }
    }
}
