//! Trait for elementary movements.
//! `ElementaryPath` handles both [Segment](segment/index.html)
//! and [Arc](arc/index.html).
use point::Point;
use utils::precision::is_almost;
use utils::Identifiable;
use tycat::Displayable;
use std::fmt::{Debug, Display};
use ordered_float::OrderedFloat;

/// `ElementaryPath` allows common functions on `Segment` and `Arc`.
pub trait ElementaryPath: Display + Displayable + Debug + Identifiable {
    /// Returns couple of start and end point.
    /// Used as basis for all default trait functions.
    fn points(&self) -> &[Point; 2];

    /// When comparing two paths p1 and p2 in a sweeping line algorithm
    /// we need to figure out which one is above which other.
    /// Of course this depends on current x position.
    /// TODO: better documentation
    fn comparison_key(&self, current_x: f64) -> (OrderedFloat<f64>,
                                                 OrderedFloat<f64>,
                                                 OrderedFloat<f64>);

    /// Returns endpoint which is not given point.
    /// pre-condition: one of our points is given point.
    ///
    /// Example
    ///
    /// ```
    /// use jimn::point::Point;
    /// use jimn::segment::Segment;
    /// use jimn::elementary_path::ElementaryPath;
    ///
    /// let points = [
    ///     Point::new(0.0, 0.0),
    ///     Point::new(1.0, 1.0),
    /// ];
    /// let segment = Segment::new(points[0], points[1]);
    /// assert_eq!(segment.endpoint_not(&points[0]), points[1]);
    /// ```
    fn endpoint_not(&self, avoided_point: &Point) -> Point {
        let points = self.points();
        if points[0] == *avoided_point {
            points[1]
        } else {
            assert_eq!(points[1], *avoided_point);
            points[0]
        }
    }

    /// returns point at start of path
    fn start(&self) -> Point {
        self.points()[0]
    }

    /// returns point at end of path
    fn end(&self) -> Point {
        self.points()[1]
    }

    /// Returns slope of line from start to end.
    /// plus or minus infinity if line is vertical.
    fn slope(&self) -> f64 {
        let &[start, end] = self.points();
        if is_almost(start.x, end.x) {
            if end.y > start.y {
                return ::std::f64::INFINITY;
            } else {
                return ::std::f64::NEG_INFINITY;
            }
        }
        (end.y - start.y) / (end.x - start.x)
    }
}
