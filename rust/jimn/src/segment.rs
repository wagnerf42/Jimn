//! Segments on the plane.
//!
//! Provides a `Segment` structure for storing oriented 2d segments.
use std::io;
use std::fs::File;
use std::cmp::{min, max};
use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
use ordered_float::NotNaN;

use point::Point;
use quadrant::{Quadrant, Shape};
use utils::precision::is_almost;
use utils::coordinates_hash::PointsHash;

/// Segment in plane
#[derive(Debug, Clone)]
pub struct Segment {
    /// start point
    pub start: Point,
    /// end point
    pub end: Point,
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
        sweeping_end.angle_with(&sweeping_start)
    }

    /// Compute intersection between two segments.
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::segment::Segment;
    /// let s1 = Segment::new(Point::new(0.0, 0.0), Point::new(2.0, 2.0));
    /// let s2 = Segment::new(Point::new(0.0, 2.0), Point::new(2.0, 0.0));
    /// let i = s1.intersection_with(&s2);
    /// assert!(i.is_some());
    /// assert!(i.unwrap().is_almost(&Point::new(1.0, 1.0)));
    /// ```
    pub fn intersection_with(&self, other: &Segment) -> Option<Point> {
        // we solve system obtained by considering the point is inside both segments.
        // p = self.start + alpha * self.direction_vector()
        // p = other.start + beta * self.direction_vector()
        let direction = self.end - self.start;
        let (x_diff, y_diff) = direction.coordinates();
        let (x_diff2, y_diff2) = (other.end - other.start).coordinates();
        let denominator = x_diff2 * y_diff - x_diff * y_diff2;
        if is_almost(denominator, 0.0) {
            None // almost parallel lines
        } else {
            let alpha = (x_diff2 * (other.start.y - self.start.y) +
                         y_diff2 * (self.start.x - other.start.x)) /
                        denominator;
            let beta = (x_diff * (other.start.y - self.start.y) +
                        y_diff * (self.start.x - other.start.x)) /
                       denominator;
            let zero = NotNaN::new(0.0).unwrap();
            let one = NotNaN::new(1.0).unwrap();
            if (is_almost(0.0, alpha) || is_almost(1.0, alpha) || (zero < alpha && alpha < one)) &&
               (is_almost(0.0, beta) || is_almost(1.0, beta) || (zero < beta && beta < one)) {
                Some(self.start + direction * alpha)
            } else {
                None
            }
        }
    }

    ///We can have several intersections in case of overlapping segments.
    ///In such a case only endpoints will count as intersections.
    ///So when computing intersections we should not return several ones.
    ///Instead we will just return next one with respect to current position.
    pub fn next_intersection_with(&self,
                                  other: &Segment,
                                  limit: &Point,
                                  rounder: &mut PointsHash)
                                  -> Option<Point> {
        if let Some(intersection) = self.intersection_with(other) {
            let result = rounder.hash_point(&intersection);
            if result <= *limit { Some(result) } else { None }
        } else if self.contains(&other.start) || other.contains(&self.start) {
            // we are overlaping, find inside points
            let (max1, min1) = self.ordered_points();
            let (max2, min2) = other.ordered_points();
            let p1 = min(max1, max2);
            let p2 = max(min1, min2);
            // *---+-*-+--* : ok
            if p1 <= *limit {
                Some(p1) // he is already hashed
            } else if p2 <= *limit {
                Some(p2) // he is already hashed
            } else {
                None
            }
        } else {
            // parallel but not aligned
            None
        }
    }

    /// Do we have given point as EXACTLY one of our endpoints ?
    pub fn has_endpoint(&self, point: &Point) -> bool {
        (self.start == *point) || (self.end == *point)
    }

    /// Translate ourselves by given vector.
    /// Adjust obtained points with rounder.
    pub fn translate(&self, vector: &Point, rounder: &mut PointsHash) -> Segment {
        Segment {
            start: rounder.hash_point(&(self.start + vector)),
            end: rounder.hash_point(&(self.end + vector)),
        }
    }
    /// Save ourselves in given file as 4 64bits little endian floats
    fn write_to_file(&self, file: &mut File) -> io::Result<()> {
        file.write_f64::<LittleEndian>(self.start.x.into_inner())?;
        file.write_f64::<LittleEndian>(self.start.y.into_inner())?;
        file.write_f64::<LittleEndian>(self.end.x.into_inner())?;
        file.write_f64::<LittleEndian>(self.end.y.into_inner())?;
        Ok(())
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

/// Save given segments by creating given file.
pub fn save_segments(filename: &str, segments: &[Segment]) -> io::Result<()> {
    let mut file = File::create(filename)?;
    for segment in segments.iter() {
        segment.write_to_file(&mut file)?;
    }
    Ok(())
}

/// Reads 4 little endian f64 from given file as a segment.
fn read_segment(file: &mut File) -> io::Result<Segment> {
    let x1 = file.read_f64::<LittleEndian>()?;
    let y1 = file.read_f64::<LittleEndian>()?;
    let x2 = file.read_f64::<LittleEndian>()?;
    let y2 = file.read_f64::<LittleEndian>()?;
    Ok(Segment::new(Point::new(x1, y1), Point::new(x2, y2)))
}

/// Reads a vector a segments from given segments file.
pub fn load_segments(filename: &str) -> io::Result<Vec<Segment>> {
    let mut file = File::open(filename)?;
    let mut segments = Vec::new();
    while let Ok(s) = read_segment(&mut file) {
        segments.push(s);
    }
    Ok(segments)
}
