//! Polygons.
//! Provides `Polygon` structure.
pub mod builder;

use bounding_box::BoundingBox;
use point::Point;
use tycat::{Displayer, Displayable};
use std::io::Write;
use utils::precision::is_almost;

/// Oriented polygons.
#[derive(Clone)]
pub struct Polygon {
    /// Vector of all points forming the edge of the polygon.
    pub points: Vec<Point>
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

    /// Simplifies polygon by removing points
    /// without losing too much precision.
    ///
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::polygon::Polygon;
    /// //note: you can add some display! to visualize the example.
    ///
    /// let complex_polygon = Polygon::new(
    ///     vec![
    ///     Point::new(-1.5, 0.2071000039577484),
    ///     Point::new(-1.29497096657753, 0.7020999744534493),
    ///     Point::new(-1.2928999662399292, 0.707099974155426),
    ///     Point::new(-1.1728129839897157, 0.9970709997415542),
    ///     Point::new(-1.1715999841690063, 1.0),
    ///     Point::new(-1.1728129839897157, 1.0029289996623993),
    ///     Point::new(-1.2928999662399292, 1.2928999662399292),
    ///     Point::new(-1.0029289996623993, 1.1728129839897157),
    ///     Point::new(-1.0, 1.1715999841690063),
    ///     Point::new(-0.7100289744138718, 1.2916869664192199),
    ///     Point::new(-0.707099974155426, 1.2928999662399292),
    ///     Point::new(-0.2121000036597252, 1.4979289996623992),
    ///     Point::new(-0.2071000039577484, 1.5),
    ///     Point::new(-0.002071000039577484, 1.005),
    ///     Point::new(0.0, 1.0),
    ///     Point::new(0.20502900391817092, 1.495),
    ///     Point::new(0.2071000039577484, 1.5),
    ///     Point::new(0.7020999744534493, 1.29497096657753),
    ///     Point::new(0.707099974155426, 1.2928999662399292),
    ///     Point::new(0.9970709997415542, 1.1728129839897157),
    ///     Point::new(1.0, 1.1715999841690063),
    ///     Point::new(1.2899709665775299, 1.2916869664192199),
    ///     Point::new(1.2928999662399292, 1.2928999662399292),
    ///     Point::new(1.2916869664192199, 1.2899709665775299),
    ///     Point::new(1.1715999841690063, 1.0),
    ///     Point::new(1.2916869664192199, 0.7100289744138718),
    ///     Point::new(1.2928999662399292, 0.707099974155426),
    ///     Point::new(1.4979289996623992, 0.2121000036597252),
    ///     Point::new(1.5, 0.2071000039577484),
    ///     Point::new(1.495, 0.20502900391817092),
    ///     Point::new(1.0, 0.0),
    ///     Point::new(1.495, -0.20502900391817092),
    ///     Point::new(1.5, -0.2071000039577484),
    ///     Point::new(1.4979289996623992, -0.2121000036597252),
    ///     Point::new(1.2928999662399292, -0.707099974155426),
    ///     Point::new(1.2916869664192199, -0.7100289744138718),
    ///     Point::new(1.1715999841690063, -1.0),
    ///     Point::new(1.2916869664192199, -1.2899709665775299),
    ///     Point::new(1.2928999662399292, -1.2928999662399292),
    ///     Point::new(1.2899709665775299, -1.2916869664192199),
    ///     Point::new(1.0, -1.1715999841690063),
    ///     Point::new(0.9970709997415542, -1.1728129839897157),
    ///     Point::new(0.707099974155426, -1.2928999662399292),
    ///     Point::new(0.7020999744534493, -1.29497096657753),
    ///     Point::new(0.2071000039577484, -1.5),
    ///     Point::new(0.20502900391817092, -1.495),
    ///     Point::new(0.0, -1.0),
    ///     Point::new(-0.002071000039577484, -1.005),
    ///     Point::new(-0.2071000039577484, -1.5),
    ///     Point::new(-0.2121000036597252, -1.4979289996623992),
    ///     Point::new(-0.707099974155426, -1.2928999662399292),
    ///     Point::new(-0.7100289744138718, -1.2916869664192199),
    ///     Point::new(-1.0, -1.1715999841690063),
    ///     Point::new(-1.0029289996623993, -1.1728129839897157),
    ///     Point::new(-1.2928999662399292, -1.2928999662399292),
    ///     Point::new(-1.1728129839897157, -1.0029289996623993),
    ///     Point::new(-1.1715999841690063, -1.0),
    ///     Point::new(-1.1728129839897157, -0.9970709997415542),
    ///     Point::new(-1.2928999662399292, -0.707099974155426),
    ///     Point::new(-1.29497096657753, -0.7020999744534493),
    ///     Point::new(-1.5, -0.2071000039577484),
    ///     Point::new(-1.005, -0.002071000039577484),
    ///     Point::new(-1.0, 0.0),
    ///     Point::new(-1.005, 0.002071000039577484)
    ///         ]);
    /// let simple_polygon = complex_polygon.simplify();
    /// assert!(simple_polygon.points.len() == 24);
    /// ```
    pub fn simplify(&self) -> Polygon {
        //triangle area
        fn area(p1: &Point, p2: &Point, p3: &Point) -> f64 {
            (p1.cross_product(p2) + p2.cross_product(p3)
             + p3.cross_product(p1)).abs()/2.0
        }

        //remove all small triangles
        //when looping on 3 consecutive points
        let new_points:Vec<Point> = self.points.iter()
            .zip(self.points.iter().cycle().skip(1))
            .zip(self.points.iter().cycle().skip(2))
            .filter_map(
                |((p1, p2), p3)|
                if area(p1, p2, p3) < 0.000001 {
                    None
                } else {
                    Some(*p2)
                }).collect();
        //now remove aligned points
        let final_points:Vec<Point> = new_points.iter()
            .zip(new_points.iter().cycle().skip(1))
            .zip(new_points.iter().cycle().skip(2))
            .filter_map(
                |((p1, p2), p3)| if p1.is_aligned_with(p2, p3) {
                    None
                } else {
                    Some(*p2)
                }).collect();
        assert!(final_points.len() > 2);
        Polygon::new(final_points)
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
