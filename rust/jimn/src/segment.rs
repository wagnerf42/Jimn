//! Segments on the plane.
//!
//! Provides a `Segment` structure for storing oriented 2d segments.
use point::Point;
use quadrant::{Quadrant, Shape};

/// Segment in plane
pub struct Segment {
    /// start point
    start: Point,
    /// end point
    end: Point,
}

impl Segment {
    /// Returns a new Segment between the two given points.
    pub fn new(start: Point, end: Point) -> Segment {
        Segment {
            start: start,
            end: end,
        }
    }

    /// Returns start and end point for sweeping line algorithm.
    /// We go for ymax to ymin and for equal ys from xmax to xmin.
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::segment::Segment;
    /// let p1 = Point::new(0.0, 3.0);
    /// let p2 = Point::new(-1.0, 4.0);
    /// let s = Segment::new(p1, p2);
    /// let (start, end) = s.ordered_points();
    /// assert!(start == p2);
    /// assert!(end == p1);
    /// ```
    pub fn ordered_points(&self) -> (Point, Point) {
        if self.start > self.end {
            (self.start, self.end)
        } else {
            (self.end, self.start)
        }
    }
}

impl Shape for Segment {
    /// Returns the smallest `Quadrant` containing us.
    fn get_quadrant(&self) -> Quadrant {
        let mut quadrant = Quadrant::new(2);
        quadrant.add(&self.start);
        quadrant.add(&self.end);
        quadrant
    }

    /// Returns svg string for tycat.
    fn svg_string(&self) -> String {
        format!("<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\"/>",
                self.start.x,
                self.start.y,
                self.end.x,
                self.end.y)
    }
}
