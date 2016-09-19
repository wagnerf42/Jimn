//! Segments on the plane.
use std::io::Write;
use bounding_box::BoundingBox;
use point::Point;
use tycat::Displayer;
use tycat::Displayable;
use utils::precision::is_almost;
use utils::Identifiable;
use elementary_path::ElementaryPath;

/// Oriented segment structure.
#[derive(Debug)]
pub struct Segment {
    points: [Point; 2]
}

impl Identifiable for Segment {}
impl Segment {
    /// Returns a new segment out of given endpoints.
    pub fn new(start: Point, end: Point) -> Segment {
        debug_assert!(!(start.is_almost(&end)));
        Segment {
            points: [start, end]
        }
    }

    /// Does given segment contain given point ?
    ///
    /// # Example
    ///
    /// ```
    /// use jimn::point::Point;
    /// use jimn::segment::Segment;
    /// let segment = Segment::new(Point::new(0.0, 0.0), Point::new(3.0, 3.0));
    /// assert!(segment.contains(&Point::new(2.0, 2.0)));
    /// ```
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
        //! let s1 = Segment::new(Point::new(0.0, 3.0), Point::new(0.0, 1.0));
        //! let s2 = Segment::new(Point::new(-3.0, 0.0), Point::new(3.0, 0.0));
        //! let result = s1.line_intersection_with(&s2);
        //! assert!(result.is_some());
        //! assert!(result.unwrap().is_almost(&Point::new(0.0, 0.0)));
        //! ```
        // solve following system :
        // intersection = start of self + alpha * direction of self
        // intersection = start of other + beta * direction of other
        let directions = [
            self.points[1] - self.points[0],
            other.points[1] - other.points[0]
        ];
        let denominator = directions[0].cross_product(&directions[1]);
        if is_almost(denominator, 0.0) {
            return None;
        }
        let start_diff = other.points[0] - self.points[0];
        let alpha = start_diff.cross_product(&directions[1]) / denominator;
        Some(self.points[0] + directions[0] * alpha)
    }

    pub fn intersection_with_segment(&self, other: &Segment) -> Option<Point> {
        //! compute intersection between given segments (if any)
        //! # Examples
        //! ```
        //! use jimn::point::Point;
        //! use jimn::segment::Segment;
        //! let s1 = Segment::new(Point::new(0.0, 3.0), Point::new(0.0, 0.0));
        //! let s2 = Segment::new(Point::new(-3.0, 1.0), Point::new(3.0, 1.0));
        //! let result = s1.intersection_with_segment(&s2);
        //! assert!(result.is_some());
        //! assert!(result.unwrap().is_almost(&Point::new(0.0, 1.0)));
        //! ```
        //TODO: reorder segments (small big) to avoid commutativity problems
        match self.line_intersection_with(other) {
            Some(i) if self.contains(&i) && other.contains(&i) => Some(i),
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
        bbox
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

impl ElementaryPath for Segment {
    fn points(&self) -> (Point, Point) {
        (self.points[0], self.points[1])
    }

    fn reverse(&self) -> Box<ElementaryPath> {
        Box::new(Segment {
            points: [self.points[1], self.points[0]]
        })
    }
}
