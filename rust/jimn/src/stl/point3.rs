//!Point3 (in 3d space) submodule for jimn.
use quadrant::{Quadrant, Shape};
use point::Point;
use utils::coordinates_hash::PointsHash;
use ordered_float::NotNaN;
use std::cmp::{max, min};

#[derive(Copy, Clone, Debug)]
/// 3D Point structure.
pub struct Point3 {
    /// X coordinate.
    pub x: NotNaN<f64>,
    /// Y coordinate.
    pub y: NotNaN<f64>,
    /// Z coordinate (height).
    pub z: NotNaN<f64>,
}

impl Point3 {
    /// Returns a new point with given coordinates.
    #[inline]
    pub fn new<T: Into<NotNaN<f64>>>(x: T, y: T, z: T) -> Point3 {
        Point3 {
            x: x.into(),
            y: y.into(),
            z: z.into(),
        }
    }

    /// Intersects segment between *self* and *end* with horizontal plane
    /// at given height.
    /// Pre-condition: one point on each side of the height.
    #[inline]
    pub fn segment_intersection(
        &self,
        end: &Point3,
        height: NotNaN<f64>,
        hasher: &mut PointsHash,
    ) -> Option<Point> {
        let lower_z = min(self.z, end.z);
        let higher_z = max(self.z, end.z);
        if height < lower_z || height > higher_z {
            None
        } else {
            let alpha = (height - self.z) / (end.z - self.z);
            let intersecting_x = self.x + alpha * (end.x - self.x);
            let intersecting_y = self.y + alpha * (end.y - self.y);
            let intersection = Point::new(intersecting_x, intersecting_y);
            Some(hasher.hash_point(&intersection))
        }
    }
}

impl Shape for Point3 {
    fn get_quadrant(&self) -> Quadrant {
        Quadrant {
            min_coordinates: vec![self.x, self.y, self.z],
            max_coordinates: vec![self.x, self.y, self.z],
        }
    }
    fn svg_string(&self) -> String {
        //flatten in 2d
        format!("<use xlink:href=\"#c\" x=\"{}\" y=\"{}\"/>", self.x, self.y)
    }
}
