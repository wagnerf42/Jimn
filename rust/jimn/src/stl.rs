//! stl submodule
//!
//! provides **Stl** class handling 3d models from stl files.
//! color information is discarded.
use point3::Point3;
use facet::Facet;

pub struct Stl {
    pub facets: Vec<Facet>
}

impl Stl {
    pub fn new(filename: &str) -> Stl {
        println!("loading stl from file {}", filename);
        Stl {
            facets: Vec::new()
        }
    }
}
