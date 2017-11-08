//! Display objects in terminology.
//!
//! Allows graphical displays under terminology.
//! Provides a **display** function for **Displayable objects**.
use std::slice;
use std::iter::Cycle;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use quadrant::{Quadrant, Shape};

/// Trick to colorize `Shapes` differently.
pub trait ColoredShape {
    /// Return quadrant for content.
    fn quadrant(&self) -> Quadrant;
    /// Return colored string for content.
    fn colored_string(&self, colors: &mut Cycle<slice::Iter<&str>>) -> String;
}

impl<S: Shape> ColoredShape for S {
    fn quadrant(&self) -> Quadrant {
        self.get_quadrant()
    }
    fn colored_string(&self, colors: &mut Cycle<slice::Iter<&str>>) -> String {
        let color = colors.next().unwrap();
        format!(
            "<g fill=\"{}\" stroke=\"{}\">{}</g>\n",
            color,
            color,
            self.svg_string()
        )
    }
}


/// One colored shapes.
pub struct UniColor {
    /// enclosing `Quadrant`
    pub quadrant: Quadrant,
    /// `String` for all content's svg
    pub string: String,
}

impl ColoredShape for UniColor {
    fn quadrant(&self) -> Quadrant {
        self.quadrant.clone()
    }
    fn colored_string(&self, colors: &mut Cycle<slice::Iter<&str>>) -> String {
        let color = colors.next().unwrap();
        format!(
            "<g fill=\"{}\" stroke=\"{}\">{}</g>\n",
            color,
            color,
            self.string
        )
    }
}

/// Multi colored shapes.
pub struct MultiColor {
    /// enclosing `Quadrant`
    pub quadrant: Quadrant,
    /// `String`s for each content's svg
    pub strings: Vec<String>,
}

impl ColoredShape for MultiColor {
    fn quadrant(&self) -> Quadrant {
        self.quadrant.clone()
    }
    fn colored_string(&self, colors: &mut Cycle<slice::Iter<&str>>) -> String {
        self.strings
            .iter()
            .map(|s| {
                let color = colors.next().unwrap();
                format!("<g fill=\"{}\" stroke=\"{}\">{}</g>\n", color, color, s)
            })
            .collect()
    }
}

#[macro_export]
/// given IntoIterator<&Shape> is to be displayed in one color.
/// use in display! macro.
macro_rules! unicolor {
    ( $x:expr ) => {
        {
            #[allow(unused_imports)]
            use $crate::quadrant::Shape;
            use $crate::tycat::UniColor;
            let mut quadrant = Quadrant::new(2);
            let mut string = String::new();
            for shape in $x.into_iter() {
                quadrant.add(shape);
                string += &shape.svg_string();
            }
            UniColor { quadrant, string }
        }
    }
}

#[macro_export]
/// given IntoIterator<&Shape> is to be displayed in several colors.
/// use in display! macro.
macro_rules! multicolor {
    ( $x:expr ) => {
        {
            #[allow(unused_imports)]
            use $crate::quadrant::Shape;
            use $crate::tycat::MultiColor;
            let mut quadrant = Quadrant::new(2);
            let mut strings = Vec::new();
            for shape in $x.into_iter() {
                quadrant.add(shape);
                strings.push(shape.svg_string());
            }
            MultiColor { quadrant, strings }
        }
    }
}

static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

/// Common colors
pub const SVG_COLORS: [&'static str; 35] = [
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
    "chartreuse",
    "darkorchid",
    "hotpink",
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
    "darkgrey",
];

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
    write!(
        svg_file,
        "viewBox=\"{} {} {} {}\" ",
        xmin,
        ymin,
        width,
        height
    )?;
    svg_file.write_all(b"xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n")?;

    // white background
    write!(svg_file, "<rect x=\"{}\" y=\"{}\" ", xmin, ymin)?;
    write!(
        svg_file,
        "width=\"{}\" height=\"{}\" fill=\"white\"/>\n",
        width,
        height
    )?;

    // circle definition and stroke size
    let xscale = 640.0 / width;
    let yscale = 480.0 / height;
    let scale = if xscale < yscale { xscale } else { yscale };
    let stroke = 3.0 / scale;
    write!(svg_file, "<defs>\n")?;
    write!(
        svg_file,
        "<symbol id=\"c\"><circle r=\"{}\"/></symbol>\n",
        2.0 * stroke
    )?;
    write!(
        svg_file,
        "<symbol id=\"a\"><line x1=\"{}\" y1=\"{}\" x2=\"0.0\" y2=\"0.0\"/>\
         <line x1=\"{}\" y1=\"{}\" x2=\"0.0\" y2=\"0.0\"/></symbol>\n",
        -3.0 * stroke,
        -3.0 * stroke,
        -3.0 * stroke,
        3.0 * stroke,
    )?;
    write!(svg_file, "</defs>\n")?;
    write!(
        svg_file,
        "<g stroke-width=\"{}\" opacity=\"0.7\">\n",
        stroke
    )?;

    for svg_string in svg_strings {
        svg_file.write_all(svg_string.as_bytes())?;
    }
    svg_file.write_all(b"</g></svg>")?;
    Command::new("tycat").arg(filename).status()?;
    Ok(())
}

#[macro_export]
/// The display macro is used to tycat to terminology `Displayable` structs
/// or vectors of displayable structs.
/// # Example
///
/// ```
/// # #[macro_use] extern crate jimn;
/// use jimn::point::Point;
/// use jimn::segment::Segment;
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
            use $crate::quadrant::Quadrant;
            use $crate::tycat::{ColoredShape, display};
            let mut quadrant = Quadrant::new(2);
            let mut svg_strings = Vec::new();
            let mut colors = $crate::tycat::SVG_COLORS.iter().cycle();
            $(
                quadrant.update(&$x.quadrant());
                svg_strings.push($x.colored_string(&mut colors));
             )*
                display(&quadrant, &svg_strings).expect("tycat failed");
        }
    }
}
