//! Handles STL files.
//!
//! Provides **Stl** class handling 3d models from stl files.
//! Color information is discarded.
mod facet;
mod point3;
use stl::facet::Facet;
use stl::point3::Point3;

/// The **Stl** structure holds a set of [facets](facet/struct.Facet.html).
pub struct Stl {
    facets: Vec<Facet>
}

impl Stl {
    /// Loads a new stl model from given file.
    pub fn new(filename: &str) -> Stl {
        println!("loading stl from file {}", filename);
        Stl {
            facets: Vec::new()
        }
    }
}
