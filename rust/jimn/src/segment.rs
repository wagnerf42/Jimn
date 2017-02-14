//! Segments on the plane.
//!
//! Provides a `Segment` structure for storing oriented 2d segments.
use ordered_float::NotNaN;
use point::Point;
use quadrant::{Quadrant, Shape};
use utils::precision::is_almost;

/// Segment in plane
#[derive(Debug)]
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

    /// Check if we contain the given point.
    /// Precondition: Do not call near endpoints or on endpoints.
    pub fn contains(&self, point: &Point) -> bool {
        assert!(!self.start.is_almost(point));
        assert!(!self.end.is_almost(point));
        is_almost(self.start.distance_to(point) + self.end.distance_to(point),
                  self.start.distance_to(&self.end))
    }

    /// Intersect with horizontal line at given y.
    /// Returns only x coordinate of intersection.
    /// Precondition: we are not a quasi-horizontal segment.
    pub fn horizontal_line_intersection(&self, y: NotNaN<f64>) -> Option<NotNaN<f64>> {
        assert!(!is_almost(self.start.y, self.end.y));
        let alpha = (y - self.start.y) / (self.end.y - self.start.y);
        if is_almost(alpha, 0) {
            Some(self.start.x)
        } else if is_almost(alpha, 1) {
            Some(self.end.x)
        } else {
            let x = self.start.x + alpha * (self.end.x - self.start.x);
            let point = Point::new(x, y);
            if self.contains(&point) { Some(x) } else { None }
        }
    }

    /// Return if we are horizontal.
    /// Pre-condition: do not call on almost horizontal segments which are not
    /// exactly horizontal.
    pub fn is_horizontal(&self) -> bool {
        assert!(self.start.y == self.end.y || !is_almost(self.start.y, self.end.y));
        self.start.y == self.end.y
    }

    /// Return angle between largest point and smallest point.
    /// This function is used for key computations in sweeping line algorithms.
    pub fn sweeping_angle(&self) -> NotNaN<f64> {
        let (sweeping_start, sweeping_end) = self.ordered_points();
        sweeping_start.angle_with(&sweeping_end)
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
