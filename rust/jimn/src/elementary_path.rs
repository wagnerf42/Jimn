//! Trait for elementary movements.
//! `ElementaryPath` handles both [Segment](segment/index.html)
//! and [Arc](arc/index.html).
use point::Point;
use utils::precision::is_almost;
use utils::Identifiable;
use tycat::Displayable;
use std::fmt::Debug;

/// `ElementaryPath` allows common functions on `Segment` and `Arc`.
pub trait ElementaryPath: Displayable + Debug + Identifiable {
    /// Returns couple of start and end point.
    /// Used as basis for all default trait functions.
    //TODO: if we return a couple we cannot iterate on it :-(
    //TODO: if we return an array reference we cannot assign it to couple :-(
    fn points(&self) -> (Point, Point);

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
        if points.0 == *avoided_point {
            points.1
        } else {
            assert_eq!(points.1, *avoided_point);
            points.0
        }
    }

    /// returns point at start of path
    fn start(&self) -> Point {
        self.points().0
    }

    /// returns point at end of path
    fn end(&self) -> Point {
        self.points().1
    }

    /// Returns slope of line from start to end.
    /// plus or minus infinity if line is vertical.
    fn slope(&self) -> f64 {
        let (start, end) = self.points();
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
