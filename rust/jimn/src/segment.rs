//! segment submodule for jimn
//!
//! provides a Segment class.
use bounding_box::BoundingBox;
use point::Point;
use tycat::Displayer;
use tycat::Displayable;
use precision::is_almost;
use std::io::Write;

pub struct Segment {
    points: [Point; 2]
}

impl Segment {
    pub fn new(start: &Point, end: &Point) -> Segment {
        Segment {
            points: [*start, *end]
        }
    }

    pub fn contains(&self, point: &Point) -> bool {
        let distance = self.points.iter().map(|&p| p.distance_to(point))
            .fold(0.0, |sum, i| sum + i);
        is_almost(distance, self.points[0].distance_to(&self.points[1]))
    }

    pub fn line_intersection_with(&self, other: &Segment) -> Option<Point> {
        //! compute intersection between lines passing through
        //! given segments (if any)
        //!
        //! # Examples
        //! ```
        //! use jimn::point::Point;
        //! use jimn::segment::Segment;
        //! let s1 = Segment(&Point::new(0.0, 3.0), &Point::new(0.0, 1.0));
        //! let s2 = Segment(&Point::new(-3.0, 0.0), &Point::new(3.0, 0.0)));
        //! let result = s1.line_intersection_with(&s2);
        //! assert!(result.is_some());
        //! asserteq!(result.unwrap() == Point::new(0.0, 0.0));
        //! ```
        println!("TODO");
        None
    }

    pub fn intersection_with_segment(&self, other: &Segment) -> Option<Point> {
        //! compute intersection between given segments (if any)
        //! # Examples
        //! ```
        //! use jimn::point::Point;
        //! use jimn::segment::Segment;
        //! let s1 = Segment(&Point::new(0.0, 3.0), &Point::new(0.0, 0.0));
        //! let s2 = Segment(&Point::new(-3.0, 1.0), &Point::new(3.0, 1.0)));
        //! let result = s1.intersection_with_segment(&s2);
        //! assert!(result.is_some());
        //! asserteq!(result.unwrap() == Point::new(0.0, 1.0));
        //! ```
        //TODO: reorder segments (small big) to avoid commutativity problems
        match self.line_intersection_with(&other) {
            Some(i) if self.contains(&i) & other.contains(&i) => Some(i),
            _ => None
        }
    }
}

impl Displayable for Segment {
    fn get_bounding_box(&self) -> BoundingBox {
        let mut bbox = BoundingBox::empty_box(2);
        for point in &self.points {
            bbox.add_point(point);
        }
        return bbox
    }

    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        let svg_coordinates:Vec<Vec<f64>> = self.points.iter()
            .map(|&p| displayer.convert_coordinates(p.coordinates()))
            .collect();
        write!(displayer.svg_file, "<line x1=\"{}\" y1=\"{}\" \
               x2=\"{}\" y2=\"{}\"",
               svg_coordinates[0][0],
               svg_coordinates[0][1],
               svg_coordinates[1][0],
               svg_coordinates[1][1])
            .expect("cannot write svg file, disk full ?");
        writeln!(displayer.svg_file, " stroke-width=\"{}\" stroke=\"{}\" \
               opacity=\"0.5\"/>",
               displayer.stroke_width, color)
            .expect("cannot write svg file, disk full ?");
    }
}
