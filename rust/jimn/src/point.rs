//! Points on the plane.
//!
//! Provides a `Point` structure for storing 2d points.
//! Points can also serve as vectors: for example point2-point1 is a point
//! which coordinates encode the direction vector of segment(point1,point2).
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Sub};
use std::fmt;

use quadrant::{Quadrant, Shape};
use utils::precision::is_almost;
use utils::float::hash_float;
use std::hash::{Hash, Hasher};


#[derive(Copy, Clone, Debug, PartialEq)]
/// 2D point structure.
/// Note that Y is defined first so that lexicographical comparisons will
/// start with y.
pub struct Point {
    /// X coordinate.
    pub x: f64,
    /// Y coordinate.
    pub y: f64,
}

#[cfg_attr(feature = "cargo-clippy", allow(derive_hash_xor_eq))]
impl Hash for Point {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_float(&self.x, state);
        hash_float(&self.y, state);
    }
}


impl Eq for Point {}
impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        self.y
            .partial_cmp(&other.y)
            .unwrap()
            .then(self.x.partial_cmp(&other.x).unwrap())
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Default for Point {
    fn default() -> Self {
        Point::new(0.0, 0.0)
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

impl Point {
    /// Returns a new Point from given coordinates.
    pub fn new(x: f64, y: f64) -> Point {
        Point { x: x, y: y }
    }

    /// Returns if the three given points are approximately aligned.
    pub fn is_aligned_with(&self, p2: &Point, p3: &Point) -> bool {
        let determinant = self.x * p2.y + self.y * p3.x + p2.x * p3.y
            - (p2.y * p3.x + self.y * p2.x + self.x * p3.y);
        determinant.abs() < 10.0_f64.powi(-5) //TODO: why 5 ?
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

    /// Look at ourselves as a vector and return a perpendicular one.
    pub fn perpendicular_vector(&self) -> Point {
        Point::new(-self.y, self.x)
    }

    /// Returns the angle needed for going towards other.
    pub fn angle_with(&self, other: &Point) -> f64 {
        let x = other.x - self.x;
        let y = other.y - self.y;
        y.atan2(x)
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

    /// Check if we are nearly the same as the other point.
    pub fn is_almost(&self, other: &Point) -> bool {
        is_almost(self.x, other.x) && is_almost(self.y, other.y)
    }

    /// Return our x and y coordinates as a tuple.
    pub fn coordinates(&self) -> (f64, f64) {
        (self.x, self.y)
    }
}

impl Shape for Point {
    fn get_quadrant(&self) -> Quadrant {
        Quadrant {
            min_coordinates: vec![self.x, self.y],
            max_coordinates: vec![self.x, self.y],
        }
    }
    fn svg_string(&self) -> String {
        format!("<use xlink:href=\"#c\" x=\"{}\" y=\"{}\"/>", self.x, self.y)
    }
}

//TODO: factorize all that stuff
impl<'a> Add<&'a Point> for Point {
    type Output = Point;
    fn add(self, other: &'a Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<'a> Add<Point> for Point {
    type Output = Point;
    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<'a> Sub<&'a Point> for Point {
    type Output = Point;
    fn sub(self, other: &'a Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl<'a> Sub<Point> for Point {
    type Output = Point;
    fn sub(self, other: Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl<'a, 'b> Sub<Point> for &'a Point {
    type Output = Point;
    fn sub(self, other: Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Mul<f64> for Point {
    type Output = Point;
    fn mul(self: Point, rhs: f64) -> Point {
        Point {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl Div<f64> for Point {
    type Output = Point;
    fn div(self: Point, rhs: f64) -> Point {
        Point {
            x: self.x / rhs,
            y: self.y / rhs,
        }
    }
}
