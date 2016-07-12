//! tycat submodule for jimn
//!
//! allows graphical displays under terminology.
//! provides a **display** function for **Displayable objects**.
extern crate std;
use bounding_box::BoundingBox;

static SVG_COLORS: [&'static str; 3] = [
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

pub fn display<T: Displayable>(objects: Vec<T>) {
//    displayer = tycat_start(objects);
//    let color_index = 0;
//    for object in &objects:
//        object.save_svg_content(displayer, svg_colors[color_index]);
//        color_index = (color_index + 1) % svg_colors.len();
//    tycat_end(display)
}
