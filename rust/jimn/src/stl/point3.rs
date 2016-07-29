//!Point3 (in 3d space) submodule for jimn.
//TODO: it would be better to have generic N-dimension points
use point::Point;

#[derive(Copy, Clone, Debug)]
/// 3D Point structure.
pub struct Point3 {
    /// X coordinate.
    pub x: f64,
    /// Y coordinate.
    pub y: f64,
    /// Z coordinate (height).
    pub z: f64
}

impl Point3 {
    /// Returns a new point with given coordinates.
    #[inline]
    pub fn new(x: f64, y: f64, z: f64) -> Point3 {
        Point3{x: x, y: y, z: z}
    }

    /// Intersects segment between *self* and *end* with horizontal plane
    /// at given height.
    /// Pre-condition: one point on each side of the height.
    #[inline]
    pub fn segment_intersection(&self, end: &Point3, height: f64) -> Point {
        let intersecting_x = 
            self.x + (height - self.z)/(end.z - self.z)*(end.x-self.x);
        let intersecting_y = 
            self.y + (height - self.z)/(end.z - self.z)*(end.y-self.y);
        //TODO: hash
        return Point::new(intersecting_x, intersecting_y);
    }
}
