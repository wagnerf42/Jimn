//! Facet submodule for jimn.
//!
//! Provides **Facet** class for handling 3D facets from stl files.
use stl::point3::Point3;

/// A facet is just a triangle in space.
pub struct Facet {
    points: [Point3; 3]
}
