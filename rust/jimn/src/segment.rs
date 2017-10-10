//! Segments on the plane.
//!
//! Provides a `Segment` structure for storing oriented 2d segments.
use std::f64::consts::PI;
use std::io;
use std::fs::File;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

use point::Point;
use quadrant::{Quadrant, Shape};
use utils::precision::is_almost;
use utils::coordinates_hash::PointsHash;

/// Segment in plane
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct Segment {
    /// start point
    pub start: Point,
    /// end point
    pub end: Point,
}

impl AsRef<Segment> for Segment {
    fn as_ref(&self) -> &Segment {
        self
    }
}

impl Segment {
    /// Returns a new Segment between the two given points.
    pub fn new(start: Point, end: Point) -> Segment {
        if start < end {
            Segment {
                start: start,
                end: end,
            }
        } else {
            Segment {
                start: start,
                end: end,
            }
        }
    }

    /// Returns reversed self.
    pub fn reverse(&self) -> Segment {
        Segment {
            start: self.end,
            end: self.start,
        }
    }

    /// Check if we contain the given point.
    /// Precondition: be careful when calling near endpoints.
    pub fn contains(&self, point: &Point) -> bool {
        is_almost(
            self.start.distance_to(point) + self.end.distance_to(point),
            self.start.distance_to(&self.end),
        )
    }

    /// Intersect with horizontal line at given y.
    /// Returns only x coordinate of intersection.
    /// Precondition: we are not a quasi-horizontal segment.
    /// There is an intersection.
    pub fn horizontal_line_intersection(&self, y: f64) -> f64 {
        assert!(!is_almost(self.start.y, self.end.y));
        let alpha = (y - self.start.y) / (self.end.y - self.start.y);
        self.start.x + alpha * (self.end.x - self.start.x)
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
    pub fn sweeping_angle(&self) -> f64 {
        let raw_angle = self.start.angle_with(&self.end);
        if raw_angle < 0.0 {
            raw_angle + PI
        } else {
            if raw_angle == PI {
                0.0
            } else {
                raw_angle
            }
        }
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
            let alpha = (x_diff2 * (other.start.y - self.start.y)
                + y_diff2 * (self.start.x - other.start.x)) / denominator;
            let beta = (x_diff * (other.start.y - self.start.y)
                + y_diff * (self.start.x - other.start.x)) / denominator;
            if (is_almost(0.0, alpha) || is_almost(1.0, alpha) || (0.0 < alpha && alpha < 1.0))
                && (is_almost(0.0, beta) || is_almost(1.0, beta) || (0.0 < beta && beta < 1.0))
            {
                Some(self.start + direction * alpha)
            } else {
                None
            }
        }
    }

    /// Translate ourselves by given vector.
    /// Adjust obtained points with rounder.
    pub fn translate(&self, vector: &Point, rounder: &mut PointsHash) -> Segment {
        Segment {
            start: rounder.hash_point(&(self.start + vector)),
            end: rounder.hash_point(&(self.end + vector)),
        }
    }

    /// Return unique key identifying line going through ourselves.
    pub fn line_key(&self) -> Point {
        if self.is_horizontal() {
            Point::new(0.0, self.start.y)
        } else {
            let alpha = (-self.start.y) / (self.end.y - self.start.y);
            let x = self.start.x + alpha * (self.end.x - self.start.x);
            Point::new(self.sweeping_angle(), x)
        }
    }

    /// Save ourselves in given file as 4 64bits little endian floats
    fn write_to_file(&self, file: &mut File) -> io::Result<()> {
        file.write_f64::<LittleEndian>(self.start.x)?;
        file.write_f64::<LittleEndian>(self.start.y)?;
        file.write_f64::<LittleEndian>(self.end.x)?;
        file.write_f64::<LittleEndian>(self.end.y)?;
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
        let middle = (self.start + self.end) / 2.0;
        format!(
            "<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\"/>\
             <use xlink:href=\"#a\" x=\"{}\" y=\"{}\" transform=\"rotate({} {} {})\"/>",
            self.start.x,
            self.start.y,
            self.end.x,
            self.end.y,
            middle.x,
            middle.y,
            self.start.angle_with(&self.end) * 360.0 / (2.0 * PI),
            middle.x,
            middle.y
        )
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
