use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::f64;
use HasCoordinates;
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

impl HasCoordinates for Point {
    fn coordinates(&self) -> Vec<f64> {
        vec![self.x, self.y]
    }
}

impl Displayable for Point {
    fn get_bounding_box(&self) -> BoundingBox {
        BoundingBox{
            min_coordinates: vec![self.x, self.y],
            max_coordinates: vec![self.x, self.y],
        }
    }

    fn save_svg_content(&self, displayer: &Displayer, color: &str) {
        println!("TODO: save svg content")
    }
}
