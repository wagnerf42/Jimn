//! Provide `TaggedPath` type for storing final paths. This is not compressed in any way.
use ElementaryPath;
use quadrant::{Quadrant, Shape};

/// Path chunk in final path.
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
                String::from("<g draw=\"red\">") + &p.svg_string() + &String::from("</g>")
            }
            TaggedPath::Fill(ref p) => {
                String::from("<g draw=\"green\">") + &p.svg_string() + &String::from("</g>")
            }
            TaggedPath::Move(ref p) => {
                String::from("<g draw=\"blue\">") + &p.svg_string() + &String::from("</g>")
            }
        }
    }
}
