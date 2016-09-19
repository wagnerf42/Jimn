//! Points on the plane.
//!
//! Provides a **Point** structure for storing 2d points.
//! Points can also serve as vectors: for example point2-point1 is a point
//! which coordinates encode the direction vector of segment(point1,point2).
use std::io::prelude::*;
use std::ops::{Add, Sub, Mul, Div};
use bounding_box::{BoundingBox, IsPoint};
use tycat::{Displayer, Displayable};
use utils::precision::is_almost;
use std::mem;
use std::hash::{Hash, Hasher};


#[derive(Copy, Clone, Debug, PartialEq)]
/// 2D point structure.
pub struct Point {
    /// X coordinate.
    pub x: f64,
    /// Y coordinate.
    pub y: f64
}

impl Eq for Point {}
impl Hash for Point {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            let as_int: u64 = mem::transmute::<f64, u64>(self.x);
            as_int.hash(state);
            let as_int: u64 = mem::transmute::<f64, u64>(self.y);
            as_int.hash(state);
        }
    }
}
impl Point {
    /// Returns a new Point from given coordinates.
    pub fn new(x: f64, y: f64) -> Point {
        Point{x: x, y: y}
    }

    /// Returns if the three given points are approximately aligned.
    pub fn is_aligned_with(&self, p2: &Point, p3: &Point) -> bool {
        let determinant = self.x*p2.y + self.y*p3.x + p2.x*p3.y
            - (p2.y*p3.x + self.y*p2.x + self.x*p3.y);
        determinant.abs() < 10.0f64.powi(-5) //TODO: why 5 ?
    }

    /// Returns if given points are almost the same
    /// (see [default precision](precision/fn.is_almost.html)).
    pub fn is_almost(&self, other: &Point) -> bool {
        is_almost(self.x, other.x) && is_almost(self.y, other.y)
    }

    /// Returns distance between given points.
    ///
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::utils::precision::is_almost;
    /// let distance = Point::new(0.0, 0.0).distance_to(&Point::new(3.0, 0.0));
    /// assert!(is_almost(distance, 3.0));
    /// ```
    pub fn distance_to(self: &Point, other: &Point) -> f64 {
        let x_diff = self.x - other.x;
        let y_diff = self.y - other.y;
        let squared_distance = x_diff * x_diff + y_diff * y_diff;
        squared_distance.sqrt()
    }

    /// Returns angle from origin between self and other.
    ///
    ///  other
    ///  (0, -1)
    ///     |
    ///     |
    ///  (0, 0) ------------ (1, 0) self
    ///
    ///
    //TODO: ?????
    pub fn angle_with(self: &Point, other: &Point) -> f64 {
        let x_diff = other.x - self.x;
        let y_diff = other.y - self.y;
        let mut raw_angle = -y_diff.atan2(x_diff);
        if raw_angle <= 0.0 {
            raw_angle += 2.0 * ::std::f64::consts::PI;
        }
        raw_angle
    }

    /// Returns vector of our coordinates.
    /// Useful for mapping.
    pub fn coordinates(&self) -> Vec<f64> {
        vec![self.x, self.y]
    }

    /// Returns cross product between vector in self and vector in other.
    ///
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::utils::precision::is_almost;
    /// let product = Point::new(1.0, 2.0).cross_product(&Point::new(2.0, 4.0));
    /// assert!(is_almost(product, 0.0));
    /// ```
    pub fn cross_product(&self, other: &Point) -> f64 {
        (self.x * other.y) - (self.y * other.x)
    }
}

//TODO: is there un-necessary cloning ?
//see: https://doc.rust-lang.org/std/ops/index.html
impl Add for Point {
    type Output = Point;
    fn add(self, other: Point) -> Point {
        Point{x : self.x + other.x, y : self.y + other.y}
    }
}

impl Sub for Point {
    type Output = Point;
    fn sub(self, other: Point) -> Point {
        Point{x: self.x - other.x, y: self.y - other.y}
    }
}

impl Mul<f64> for Point {
    type Output = Point;
    fn mul(self: Point, rhs: f64) -> Point {
        Point{x: self.x * rhs, y: self.y * rhs}
    }
}

impl Div<f64> for Point {
    type Output = Point;
    fn div(self: Point, rhs: f64) -> Point {
        Point{x: self.x / rhs, y: self.y / rhs}
    }
}

impl Displayable for Point {
    fn get_bounding_box(&self) -> BoundingBox {
        BoundingBox {
            min_coordinates: vec![self.x, self.y],
            max_coordinates: vec![self.x, self.y],
        }
    }

    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        let svg_coordinates = displayer
            .convert_coordinates(self.coordinates());
        //TODO: fill format with one vector ?
        write!(displayer.svg_file, "<circle cx=\"{}\" cy=\"{}\"",
               svg_coordinates[0], svg_coordinates[1])
            .expect("cannot write svg file, disk full ?");
        writeln!(displayer.svg_file, " r=\"{}\" fill=\"{}\" opacity=\"0.5\"/>",
               2.0*displayer.stroke_width, color)
            .expect("cannot write svg file, disk full ?");
    }
}

//TODO: smart way to avoid duplication ?
impl IsPoint for Point {
    fn coordinates(&self) -> Vec<f64> {
        vec![self.x, self.y]
    }
}
