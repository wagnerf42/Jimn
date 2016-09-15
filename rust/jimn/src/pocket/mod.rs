//! Pockets.
//! Provides `Pocket` structure holding a set of elementary paths delimiting
//! a plane area by either segments or arcs.
pub mod builder;

use bounding_box::BoundingBox;
use tycat::{Displayer, Displayable};
use elementary_path::ElementaryPath;
use polygon::Polygon;

/// A `Pocket` holds a contiguous area delimited by segments or arcs.
/// It is obtained after computing accessible areas for milling inside
/// a polygon.
pub struct Pocket {
    paths: Vec<Box<ElementaryPath>>
}

impl Pocket {
    /// Creates a new pocket of given vector of boxed elementary paths.
    pub fn new(paths: Vec<Box<ElementaryPath>>) -> Pocket {
        Pocket {
            paths: paths
        }
    }

    /// Creates a new Polygon joining all start points of paths in self.
    pub fn to_polygon(&self) -> Polygon {
        Polygon::new(self.paths.iter().map(|p| p.start()).collect())
    }
}

impl Displayable for Pocket {
    fn get_bounding_box(&self) -> BoundingBox {
        let mut bbox = BoundingBox::empty_box(2);
        for path in &self.paths {
            bbox.update(&path.get_bounding_box());
        }
        bbox
    }

    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        for path in &self.paths {
            path.save_svg_content(displayer, color);
        }
    }
}
