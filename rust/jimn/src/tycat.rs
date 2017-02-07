//! Display objects in terminology.
//!
//! Allows graphical displays under terminology.
//! Provides a **display** function for **Displayable objects**.
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use quadrant::Quadrant;

static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

const SVG_COLORS: [&'static str; 37] = [
    "red",
    "green",
    "blue",
    "purple",
    "orange",
    "saddlebrown",
    "mediumseagreen",
    "darkolivegreen",
    "lightskyblue",
    "dimgray",
    "mediumpurple",
    "midnightblue",
    "olive",
    "chartreuse",
    "darkorchid",
    "hotpink",
    "darkred",
    "peru",
    "goldenrod",
    "mediumslateblue",
    "orangered",
    "darkmagenta",
    "darkgoldenrod",
    "mediumslateblue",
    "firebrick",
    "palegreen",
    "royalblue",
    "tan",
    "tomato",
    "springgreen",
    "pink",
    "orchid",
    "saddlebrown",
    "moccasin",
    "mistyrose",
    "cornflowerblue",
    "darkgrey"
];

/// tycat given svg strings bounded by given quadrant.
pub fn display(quadrant: &Quadrant, svg_strings: &Vec<String>) {
    let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
    let filename = format!("/tmp/test-{}.svg", file_number);
    println!("[{}]", file_number);
    let mut svg_file = File::create(&filename).expect("cannot create svg file");
    svg_file.write_all(b"<svg width=\"640\" height=\"480\">")
        .expect("cannot write svg file, disk full ?");
    for svg_string in svg_strings {
        svg_file.write(svg_string.as_bytes());
    }
    svg_file.write_all(b"</svg>")
        .expect("cannot write svg file, disk full ?");
    Command::new("tycat").arg(filename).status().expect("tycat failed");
}

#[macro_export]
/// The display macro is used to tycat to terminology **Displayable** structs
/// or vectors of displayable structs.
/// # Example
///
/// ```
/// # #[macro_use] extern crate jimn;
/// use jimn::point::Point;
/// use jimn::segment::Segment;
/// use jimn::tycat::{Displayable, display};
/// # fn main() {
/// let single_point = Point::new(3.0, 2.0); //NOTE: declared BEFORE display
/// let segment = Segment::new(Point::new(1.0, 1.0), Point::new(0.0, -1.0));
/// display!(single_point, segment);
/// # }
/// ```
macro_rules! display {
    ( $($x:expr ), +) => {
        {
        let mut quadrant = Quadrant::new(2);
        let mut svg_strings = Vec::new();
        $(
            {
                let small_quadrant = $x.get_quadrant();
                quadrant.update(&small_quadrant);
            }
            svg_strings.push($x.svg_string());
        )*
        display(&quadrant, &svg_strings);
        }
    }
}
