//!Point3 (in 3d space) submodule for jimn.
use quadrant::{Quadrant, Shape};
use point::Point;
use utils::coordinates_hash::PointsHash;

#[derive(Copy, Clone, Debug)]
/// 3D Point structure.
pub struct Point3 {
    /// X coordinate.
    pub x: f64,
    /// Y coordinate.
    pub y: f64,
    /// Z coordinate (height).
    pub z: f64,
}

impl Point3 {
    /// Returns a new point with given coordinates.
    #[inline]
    pub fn new(x: f64, y: f64, z: f64) -> Point3 {
        Point3 { x, y, z }
    }

    /// Intersects segment between *self* and *end* with horizontal plane
    /// at given height.
    /// Pre-condition: one point on each side of the height.
    #[inline]
    pub fn segment_intersection(
        &self,
        end: &Point3,
        height: f64,
        hasher: &mut PointsHash,
    ) -> Option<Point> {
        let (lower_z, higher_z) = if self.z < end.z {
            (self.z, end.z)
        } else {
            (end.z, self.z)
        };
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
