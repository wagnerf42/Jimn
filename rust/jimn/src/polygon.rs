//! Polygons.
//! Provides `Polygon` structure.
use bounding_box::BoundingBox;
use point::Point;
use tycat::{Displayer, Displayable};
use std::io::Write;

/// Oriented polygons.
pub struct Polygon {
    points: Vec<Point>
}

impl Polygon {
    /// Create polygon out of given points vector.
    pub fn new(points: Vec<Point>) -> Polygon {
        Polygon {
            points: points
        }
    }
}

impl Displayable for Polygon {
    fn get_bounding_box(&self) -> BoundingBox {
        //TODO: next line is not ok since add_point does not return a bbox
        //can we change it to return a bbox or does it incur extra copies ?
        //self.points.iter().fold(BoundingBox::empty_box(2), |bbox, point| bbox.add_point(point))
        let mut bbox = BoundingBox::empty_box(2);
        for point in &self.points {
            bbox.add_point(point);
        }
        bbox
    }
    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        let coordinates: Vec<Vec<f64>> = self.points.iter()
            .map(|point| displayer.convert_coordinates(point.coordinates()))
            .collect();
        let strings: Vec<String> = coordinates.iter()
            .map(|c| format!("{},{}", c[0], c[1])).collect();
        let points_string = strings.join(" ");
        writeln!(displayer.svg_file, 
                 "<polygon points=\"{}\" \
                 style=\"fill:{};stroke:none;opacity:0.5\"/>",
                 points_string, color
                 ).expect("cannot write svg file, disk full ?");
    }
}
