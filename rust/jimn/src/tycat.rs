//! Display objects in terminology.
//!
//! Allows graphical displays under terminology.
//! Provides a **display** function for **Displayable objects**.
use std::io;
use std::cmp::min;
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use quadrant::Quadrant;
use ordered_float::NotNaN;

static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

const SVG_COLORS: [&'static str; 37] = ["red",
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
                                        "darkgrey"];

/// tycat given svg strings bounded by given quadrant.
pub fn display(quadrant: &Quadrant, svg_strings: &[String]) -> io::Result<()> {
    let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
    let filename = format!("/tmp/test-{}.svg", file_number);
    println!("[{}]", file_number);
    let mut svg_file = File::create(&filename)?;

    // write header
    svg_file.write_all(b"<svg width=\"640\" height=\"480\" ")?;
    let (xmin, xmax) = quadrant.limits(0);
    let (ymin, ymax) = quadrant.limits(1);
    let width = xmax - xmin;
    let height = ymax - ymin;
    write!(svg_file,
           "viewBox=\"{} {} {} {}\" ",
           xmin,
           ymin,
           width,
           height)?;
    svg_file.write_all(b"xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n")?;

    // white background
    write!(svg_file, "<rect x=\"{}\" y=\"{}\" ", xmin, ymin)?;
    write!(svg_file,
           "width=\"{}\" height=\"{}\" fill=\"white\"/>\n",
           width,
           height)?;

    // circle definition and stroke size
    let xscale = NotNaN::new(640.0).unwrap() / width;
    let yscale = NotNaN::new(480.0).unwrap() / height;
    let scale = min(xscale, yscale);
    let stroke = 3.0 / scale.into_inner();
    write!(svg_file,
           "<defs><symbol id=\"c\"><circle r=\"{}\"/></symbol></defs>\n",
           2.0 * stroke)?;
    write!(svg_file,
           "<g stroke-width=\"{}\" opacity=\"0.7\">\n",
           stroke)?;

    for (svg_string, color) in svg_strings.iter().zip(SVG_COLORS.iter()) {
        write!(svg_file, "<g fill=\"{}\" stroke=\"{}\">\n", color, color)?;
        svg_file.write_all(svg_string.as_bytes())?;
        svg_file.write_all(b"\n</g>\n")?;
    }
    svg_file.write_all(b"</g></svg>")?;
    Command::new("tycat").arg(filename).status()?;
    Ok(())
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
/// use jimn::quadrant::Quadrant;
/// use jimn::tycat::display;
/// # fn main() {
/// let p1 = Point::new(3.0, 2.0);
/// let p2 = Point::new(1.0, 1.0);
/// let p3 = Point::new(5.0, 1.0);
/// let s = Segment::new(p2, p3);
/// display!(p1, p2, p3, s);
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
        display(&quadrant, &svg_strings).expect("tycat failed");
        }
    }
}
