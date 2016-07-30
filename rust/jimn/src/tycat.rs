//! Display objects in terminology.
//!
//! Allows graphical displays under terminology.
//! Provides a **display** function for **Displayable objects**.
extern crate std;
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use bounding_box::BoundingBox;

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

/// a `Displayer` object stores all information for building svg images
/// of vectors of different `Displayable` objects.
/// `Displayable` objects need to implement `save_svg_content`
/// which uses the `Displayer` to write appropriate coordinates in current
/// svg file.
pub struct Displayer {
    svg_dimensions: Vec<f64>,
    margin: f64,
    /// file holding currently built svg image
    pub svg_file: std::fs::File,
    min_coordinates: [f64; 2],
    max_coordinates: [f64; 2],
    margins: Vec<f64>,
    stretch: f64,
    /// advised width of stroke for svg lines.
    pub stroke_width: f64
}

/// implement this trait for any struct which you want to display graphically.
pub trait Displayable {
    /// get **BoundingBox** around self. this box is then used by displayer to
    /// shift and stretch all displayed objects on screen.
    fn get_bounding_box(&self) -> BoundingBox;
    /// add svg code for self into file currently being built by **Displayer**.
    /// everything drawn should be of given color.
    fn save_svg_content(&self, displayer: &mut Displayer, color: &str);
}

///vectors of *Displayable* are also *Displayable*.
impl<T: Displayable> Displayable for  Vec<T> {
    fn get_bounding_box(&self) -> BoundingBox {
        let mut bbox = BoundingBox::empty_box(2);
        for content in self {
            bbox.update(&content.get_bounding_box());
        }
        bbox
    }
    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        for content in self {
            content.save_svg_content(displayer, color);
        }
    }
}

impl Displayer {
    /// return a new **Displayer**.
    /// this displayer is auto-calibrated to display given vector of **Displayable**.
    pub fn new(filename : &str, objects: &[&Displayable]) -> Displayer {
        let file = File::create(filename).expect("failed opening file for tycat");
        let mut global_box = BoundingBox::empty_box(2);
        for object in objects {
            let bbox = object.get_bounding_box();
            global_box.update(&bbox);
        }

        let mut displayer = Displayer {
            svg_dimensions: vec![800.0, 600.0],
            margin: 20.0,
            svg_file: file,
            min_coordinates: [global_box.min_coordinates[0], global_box.min_coordinates[1]],
            max_coordinates: [global_box.max_coordinates[0], global_box.max_coordinates[1]],
            margins: vec![0.0, 0.0],
            stretch: 0.0,
            stroke_width: 0.0
        };

        displayer.calibrate();

        writeln!(
            displayer.svg_file,
            "<svg width=\"{}\" height=\"{}\">\n",
            displayer.svg_dimensions[0],
            displayer.svg_dimensions[1]
        ).expect("cannot write svg file, disk full ?");
        writeln!(
            displayer.svg_file,
            "<rect x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" fill=\"white\"/>\n",
            displayer.svg_dimensions[0],
            displayer.svg_dimensions[1]
        ).expect("cannot write svg file, disk full ?");
        displayer
    }

    /// convert given coordinates to display in svg file we are building
    pub fn convert_coordinates(&self, coordinates: Vec<f64>) -> Vec<f64> {
        let relative_coordinates: Vec<f64> = coordinates.iter()
            .zip(self.min_coordinates.iter())
            .map(|(&a, &b)| a - b).collect();
        self.margins.iter().zip(relative_coordinates.iter())
            .map(|(&a, &b)| a+b*self.stretch).collect()
    }

    fn calibrate(&mut self) {
        //TODO: can we avoid collecting here ??
        let dimensions: Vec<f64> = self.max_coordinates.iter()
            .zip(self.min_coordinates.iter())
            .map(|(&a, &b)| a-b).collect();
        let real_dimensions: Vec<f64> = self.svg_dimensions.iter()
            .map(|&d| d-2.0*self.margin).collect();
        //TODO: avoid dimension by 0
        let stretches: Vec<f64> = dimensions.iter().zip(real_dimensions.iter())
            .map(|(&a, &b)| b/a).collect();
        self.stretch = stretches.iter().cloned().fold(std::f64::INFINITY, f64::min);
        self.stroke_width = self.svg_dimensions.iter().cloned()
            .fold(std::f64::INFINITY, f64::min) / 200.0;
        self.margins = real_dimensions.iter().zip(dimensions.iter()).
            map(|(&real, &fake)| (real - fake*self.stretch)/2.0 + self.margin)
            .collect();
    }
}

/// display vector of displayable objects (one color each)
pub fn display(objects: &[&Displayable]) {
    let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
    let filename = format!("/tmp/test-{}.svg", file_number);
    println!("[{}]", file_number);
    {
        let mut displayer = Displayer::new(filename.as_str(), objects);
        let mut color_index = 0;
        for object in objects {
            object.save_svg_content(&mut displayer, SVG_COLORS[color_index]);
            color_index = (color_index + 1) % SVG_COLORS.len();
        }
        displayer.svg_file.write_all(b"</svg>")
            .expect("cannot write svg file, disk full ?");
    }
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
/// # fn main() {
/// let single_point = Point::new(3.0, 2.0); //NOTE: declared BEFORE display
/// let segment = Segment::new(Point::new(1.0, 1.0), Point::new(0.0, -1.0));
/// display!(single_point, segment);
/// # }
/// ```
macro_rules! display {
    ( $($x:expr ), +) => {
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push(&$x as &jimn::tycat::Displayable);
        )*
        jimn::tycat::display(&temp_vec);
    }
}
