//! Segments on the plane.
use std::io::Write;
use std::f64::consts::PI;
use std::fmt;
use ordered_float::OrderedFloat;

use bounding_box::BoundingBox;
use point::Point;
use tycat::{Displayer, Displayable};
use tree::treap::Positionable;
use utils::precision::is_almost;
use utils::Identifiable;

/// Oriented segment structure.
#[derive(Debug)]
pub struct Segment {
    /// Segment's two endpoints (start and end).
    pub points: [Point; 2]
}

impl Identifiable for Segment {}
impl Default for Segment {
    fn default() -> Segment {
        Segment::new(Point::new(0.0, 0.0), Point::new(1.0, 0.0))
    }
}
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
    /// Returns `Segment` going from end to start.
    pub fn reverse(&self) -> Segment {
        Segment {
            points: [self.points[1], self.points[0]]
        }
    }

    /// Intersects line going through segment
    /// with vertical line going through given x (returns y).
    /// If we are a vertical segment at given x, returns y with highest value.
    /// If we are a vertical segment not at given x, returns None.
    ///
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::segment::Segment;
    /// use jimn::utils::precision::is_almost;
    /// let s = Segment::new(Point::new(0.0, 0.0), Point::new(4.0, 8.0));
    /// let y = s.vertical_intersection_at(2.0).unwrap();
    /// assert!(is_almost(y, 4.0));
    /// ```
    pub fn vertical_intersection_at(&self, intersecting_x: f64)
        -> Option<f64> {
        let p1 = self.points[0];
        let p2 = self.points[1];
        if is_almost(p1.x, p2.x) {
            if is_almost(intersecting_x, p1.x) {
                if p1.y < p2.y { Some(p1.y) } else { Some(p2.y) }
            } else {
                None
            }
        }
        else if is_almost(intersecting_x, p1.x) { Some(p1.y) }
        else if is_almost(intersecting_x, p2.x) { Some(p2.y) }
        else {
            let slope = (p2.y - p1.y) / (p2.x - p1.x);
            Some(p1.y + slope*(intersecting_x - p1.x))
        }
    }

    ///Returns endpoint of segment.
    pub fn end(&self) -> Point {
        self.points[1]
    }

    ///Returns startpoint of segment.
    pub fn start(&self) -> Point {
        self.points[0]
    }

    ///Returns endpoint not the given one in segment.
    pub fn endpoint_not(&self, avoided_point: &Point) -> Point {
        if self.points[0] == *avoided_point {
            self.points[1]
        } else {
            assert_eq!(*avoided_point, self.points[1]);
            self.points[0]
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

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}, {}]", self.points[0], self.points[1])
    }
}

impl Positionable for Segment {
    fn comparison_key(&self, current_x: f64)
        -> (OrderedFloat<f64>, OrderedFloat<f64>, OrderedFloat<f64>) {
        if cfg!(debug_assertions) {
            let mut x_coordinates:Vec<f64> = self.points.iter()
                .map(|p| p.x).collect();
            x_coordinates.sort_by(|a, b| a.partial_cmp(b).unwrap());
            assert!(x_coordinates[0] <= current_x);
            assert!(current_x <= x_coordinates[1]);
        }
        let point_key =
            Point::new(current_x,
                       self.vertical_intersection_at(current_x).unwrap());
        //TODO: round ?
        let terminal_angle = self.points[0].angle_with(&self.points[1]) % PI;
        if self.points[1].is_almost(&point_key) {
            (
                OrderedFloat(point_key.y),
                OrderedFloat(-terminal_angle),
                OrderedFloat(-terminal_angle)
            )
        } else {
            (
                OrderedFloat(point_key.y),
                OrderedFloat(terminal_angle),
                OrderedFloat(terminal_angle)
            )
        }
    }
}
