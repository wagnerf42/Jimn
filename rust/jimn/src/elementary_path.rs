//! Trait for elementary movements.
//! `ElementaryPath` handles both [Segment](segment/index.html)
//! and [Arc](arc/index.html).
use point::Point;
use utils::precision::is_almost;
use tycat::Displayable;

/// `ElementaryPath` allows common functions on `Segment` and `Arc`.
pub trait ElementaryPath: Displayable {
    /// Returns array of start and end point.
    /// Used as basis for all default trait functions.
    fn points(&self) -> &[Point; 2];
    /// Returns a reversed path of same type.
    /// Since we cannot return a type of unknown size we must box it.
    /// TODO: solution ?
    fn reverse(&self) -> Box<ElementaryPath>;

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
    /// assert_eq!(segment.endpoint_not(points[0]), points[1]);
    /// ```
    fn endpoint_not(&self, avoided_point: Point) -> Point {
        let points = self.points();
        if points[0] == avoided_point {
            return points[1];
        } else {
            assert_eq!(points[1], avoided_point);
            return points[0];
        }
    }

    /// Returns slope of line from start to end.
    /// plus or minus infinity if line is vertical.
    fn slope(&self) -> f64 {
        let points = self.points();
        if is_almost(points[0].x, points[1].x) {
            if points[1].y > points[0].y {
                return ::std::f64::INFINITY;
            } else {
                return ::std::f64::NEG_INFINITY;
            }
        }
        (points[1].y - points[0].y) / (points[1].x - points[0].x)
    }
}
