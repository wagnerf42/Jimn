//!Point3 (in 3d space) submodule for jimn.
//TODO: it would be better to have generic N-dimension points
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
}
