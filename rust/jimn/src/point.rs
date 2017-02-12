//! Points on the plane.
//!
//! Provides a `Point` structure for storing 2d points.
//! Points can also serve as vectors: for example point2-point1 is a point
//! which coordinates encode the direction vector of segment(point1,point2).
use std::ops::{Add, Sub, Mul, Div};

use quadrant::{Quadrant, Shape};
use ordered_float::NotNaN;


#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// 2D point structure.
pub struct Point {
    /// X coordinate.
    pub x: NotNaN<f64>,
    /// Y coordinate.
    pub y: NotNaN<f64>,
}

impl Point {
    /// Returns a new Point from given coordinates.
    pub fn new<T: Into<NotNaN<f64>>>(x: T, y: T) -> Point {
        Point {
            x: x.into(),
            y: y.into(),
        }
    }

    /// Returns if the three given points are approximately aligned.
    pub fn is_aligned_with(&self, p2: &Point, p3: &Point) -> bool {
        let determinant = self.x * p2.y + self.y * p3.x + p2.x * p3.y -
                          (p2.y * p3.x + self.y * p2.x + self.x * p3.y);
        determinant.abs() < 10.0f64.powi(-5) //TODO: why 5 ?
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

    /// Returns vector of our coordinates.
    /// Useful for mapping or arithmetic operations.
    pub fn to_vector(&self) -> Vec<NotNaN<f64>> {
        vec![self.x, self.y]
    }

    /// Returns the angle needed for going towards other.
    pub fn angle_with(&self, other: &Point) -> NotNaN<f64> {
        let x = other.x - self.x;
        let y = other.y - self.y;
        NotNaN::new(y.atan2(x.into_inner())).unwrap()
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
    pub fn cross_product(&self, other: &Point) -> NotNaN<f64> {
        (self.x * other.y) - (self.y * other.x)
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

//TODO: is there un-necessary cloning ?
//see: https://doc.rust-lang.org/std/ops/index.html
impl Add for Point {
    type Output = Point;
    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Point {
    type Output = Point;
    fn sub(self, other: Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl<T: Into<NotNaN<f64>>> Mul<T> for Point {
    type Output = Point;
    fn mul(self: Point, rhs: T) -> Point {
        let factor = rhs.into();
        Point {
            x: self.x * factor,
            y: self.y * factor,
        }
    }
}

impl<T: Into<NotNaN<f64>>> Div<T> for Point {
    type Output = Point;
    fn div(self: Point, rhs: T) -> Point {
        let divisor = rhs.into();
        Point {
            x: self.x / divisor,
            y: self.y / divisor,
        }
    }
}
