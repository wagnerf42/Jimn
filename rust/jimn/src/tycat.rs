//! tycat submodule for jimn
//!
//! allows graphical displays under terminology.
//! provides a **display** function for **Displayable objects**.
extern crate std;
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use bounding_box::BoundingBox;
use float_min;

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
    svg_dimensions: Vec<f64>,
    margin: f64,
    pub svg_file: std::fs::File,
    min_coordinates: [f64; 2],
    max_coordinates: [f64; 2],
    margins: Vec<f64>,
    stretch: f64,
    pub stroke_width: f64
}

pub trait Displayable {
    fn get_bounding_box(&self) -> BoundingBox;
    fn save_svg_content(&self, displayer: &mut Displayer, color: &str);
}

impl Displayer {
    pub fn new(filename : &str, objects: &Vec<&Displayable>) -> Displayer {
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
        return displayer;
    }

    pub fn convert_coordinates(&self, coordinates: Vec<f64>) -> Vec<f64> {
        let relative_coordinates: Vec<f64> = coordinates.iter()
            .zip(self.min_coordinates.iter())
            .map(|(&a, &b)| a - b).collect();
        return self.margins.iter().zip(relative_coordinates.iter())
            .map(|(&a, &b)| a+b*self.stretch).collect();
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
        self.stretch = float_min(&stretches);
        self.stroke_width = float_min(&self.svg_dimensions) / 200.0;
        self.margins = real_dimensions.iter().zip(dimensions.iter()).
            map(|(&real, &fake)| (real - fake*self.stretch)/2.0 + self.margin)
            .collect();
    }
}

/// display vector of displayable objects (one color each)
pub fn display(objects: &Vec<&Displayable>) {
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
