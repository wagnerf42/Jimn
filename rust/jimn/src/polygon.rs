//! Polygons.
//! Provides `Polygon` structure.
use bounding_box::BoundingBox;
use point::Point;
use tycat::{Displayer, Displayable};
use std::io::Write;
use utils::precision::is_almost;

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
    
    /// Returns area taken by polygon.
    /// Negative or Positive depending on orientation.
    pub fn area(&self) -> f64 {
        self.points.iter().zip(self.points.iter().cycle().skip(1))
            .map(|(p1, p2)| p1.cross_product(p2))
            .fold(0.0, |sum, a| sum + a)
    }

    /// Returns if polygon is oriented clockwise (with respect to svg
    /// orientation)
    pub fn is_oriented_clockwise(&self) -> bool {
        let area = self.area();
        assert!(!is_almost(area, 0.0)); // flat or crossing polygon
        area > 0.0
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
