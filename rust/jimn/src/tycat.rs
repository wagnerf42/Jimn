//! tycat submodule for jimn
//!
//! allows graphical displays under terminology.
//! provides a **display** function for **Displayable objects**.
extern crate std;
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::{Command, Stdio};
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

pub struct Displayer {
    svg_dimensions: [f64; 2],
    margin: f64,
    filename: String,
    svg_file: std::fs::File,
    min_coordinates: [f64; 2],
    max_coordinates: [f64; 2]
}

pub trait Displayable {
    fn get_bounding_box(&self) -> BoundingBox;
    fn save_svg_content(&self, displayer: &Displayer, color: &str);
}

impl Displayer {
    pub fn new<T: Displayable>(filename : &str, objects: &Vec<T>) -> Displayer {
        let file = File::create(filename).expect("failed opening file for tycat");
        let mut global_box = BoundingBox::empty_box(2);
        for object in objects {
            let bbox = object.get_bounding_box();
            global_box.update(&bbox);
        }

        let mut displayer = Displayer {
            svg_dimensions: [800.0, 600.0],
            margin: 20.0,
            filename: filename.to_string(),
            svg_file: file,
            min_coordinates: [global_box.min_coordinates[0], global_box.min_coordinates[1]],
            max_coordinates: [global_box.max_coordinates[0], global_box.max_coordinates[1]],
        };

        writeln!(
            displayer.svg_file,
            "<svg width=\"{}\" height=\"{}\">\n",
            displayer.svg_dimensions[0],
            displayer.svg_dimensions[1]
        ).unwrap();

        writeln!(
            displayer.svg_file,
            "<rect x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" fill=\"white\"/>\n",
            displayer.svg_dimensions[0],
            displayer.svg_dimensions[1]
        ).unwrap();
        return displayer;
    }
}

/// display vector of displayable objects (one color each)
pub fn display<T: Displayable>(objects: &Vec<T>) {
    let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
    let filename = format!("/tmp/test-{}.svg", file_number);
    println!("[{}]", file_number);
    {
        let mut displayer = Displayer::new(filename.as_str(), objects);
        let mut color_index = 0;
        for object in objects {
            object.save_svg_content(&displayer, SVG_COLORS[color_index]);
            color_index = (color_index + 1) % SVG_COLORS.len();
        }
        displayer.svg_file.write_all(b"</svg>");
    }
    Command::new("tycat").arg(filename).status().expect("tycat failed");
}
