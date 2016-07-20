//! point submodule for jimn
//!
//! provides **Point** class for storing 2d points.
//! points can also serve as vectors: for example point2-point1 is a point
//! which coordinates encode the direction vector of segment(point1,point2).
use std::io::prelude::*;
use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::f64;
use bounding_box::BoundingBox;
use tycat::Displayer;
use tycat::Displayable;

#[derive(Copy, Clone)]
pub struct Point {
    pub x: f64,
    pub y: f64
}

impl Point {
    pub fn new(x: f64, y: f64) -> Point {
        Point{x: x, y: y}
    }

    /// return distance between given points
    ///
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::precision::is_almost;
    /// let distance = Point::new(0.0, 0.0).distance_to(&Point::new(3.0, 0.0));
    /// assert!(is_almost(distance, 3.0));
    /// ```
    pub fn distance_to(self: &Point, other: &Point) -> f64 {
        let x_diff = self.x - other.x;
        let y_diff = self.y - other.y;
        let squared_distance = x_diff * x_diff + y_diff * y_diff;
        squared_distance.sqrt()
    }

    pub fn angle_with(self: &Point, other: &Point) -> f64 {
        let x_diff = other.x - self.x;
        let y_diff = other.y - self.y;
        let mut raw_angle = -y_diff.atan2(x_diff);
        if raw_angle <= 0.0 {
            raw_angle += 2.0 * f64::consts::PI;
        }
        raw_angle
    }

    pub fn coordinates(&self) -> Vec<f64> {
        vec![self.x, self.y]
    }

    pub fn cross_product(&self, other: &Point) -> f64 {
        //! return cross product between vector in self and vector in other
        //!
        //! # Example
        //! ```
        //! use jimn::Point;
        //! use jimn::precision::is_almost;
        //! let product = Point::new(0.0, 1.0).cross_product(&Point::new(1.0, 0.0));
        //! assert!(is_almost(product, 0.0));
        //! ```
        return (self.x * other.y) - (self.y * other.x);
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
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
