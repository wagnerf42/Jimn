//! tycat submodule for jimn
//!
//! allows graphical displays under terminology.
//! provides a **display** function for **Displayable objects**.
extern crate std;
use std::io::prelude::*;
use std::fs::File;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use bounding_box::BoundingBox;
static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

const SVG_COLORS: [&'static str; 3] = [
    "red",
    "green",
    "blue"
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
    fn save_svg_content(&self, displayer: &Displayer);
}

impl Displayer {
    pub fn new<T: Displayable>(filename : &str, objects: &Vec<T>) -> Displayer {
        //TODO: add non working try
        let mut file = try!(File::create(filename));
        let mut global_box = BoundingBox::empty_box(2);
        for object in objects {
            let bbox = object.get_bounding_box();
            global_box.update(&bbox);
        }
            
        let displayer = Displayer {
            svg_dimensions: [800.0, 600.0],
            margin: 20.0,
            filename: filename.to_string(),
            svg_file: file,
            min_coordinates: [global_box.min_coordinates[0], global_box.min_coordinates[1]],
            max_coordinates: [global_box.max_coordinates[0], global_box.max_coordinates[1]],
        };
        displayer.svg_file.write(
            format!("<svg width=\"{}\" height=\"{}\">\n",
                    displayer.svg_dimensions[0],
                    displayer.svg_dimensions[1])
            );
        return displayer;
    }
}

pub fn tycat_start<T: Displayable>(objects: &Vec<T>) -> Displayer {
    let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
    println!("[{}]", file_number);
    //TODO: create dir
    let filename = "/tmp/test.svg";
    let displayer = Displayer::new(filename, objects);
    return displayer;
}

/// display vector of displayable objects (one color each)
pub fn display<T: Displayable>(objects: &Vec<T>) {
    let displayer = tycat_start(objects);
//    let color_index = 0;
//    for object in &objects:
//        object.save_svg_content(displayer, svg_colors[color_index]);
//        color_index = (color_index + 1) % svg_colors.len();
//    tycat_end(display)
}
