//! **Jimn** is a geometry module intended for computing paths for CNC machines.
//!
//! It however can still be used as a standalone module for doing geometry.
//! You can find standard graphical objects xxx in sub-modules `jimn::xxx` like :
//!
//! * point
//! * segment
//! * arc
//! * polygon
//! * circle
//! * holed polygon
//!
//! Jimn is intended to work with the terminology terminal emulator from
//! enlightenment. Under terminology any object or list of objects can be
//! graphically displayed using the [display!](macro.display!.html) macro from `jimn::tycat`.
//! Many graphical examples are available in the *examples* sub-directory.
//! Compile them with cargo !

#![deny(missing_docs)]
#![feature(btree_range, collections_bound, plugin)]
#![plugin(clippy)]

extern crate byteorder;
#[macro_use]
pub mod tycat;
pub mod tree;
pub mod utils;
pub mod elementary_path;
pub mod bounding_box;
pub mod point;
pub mod pocket;
pub mod segment;
pub mod arc;
pub mod stl;
pub mod polygon;
pub mod sweeping_lines;

use stl::Stl;
use tycat::{Displayable, display};
use polygon::builder::build_polygons;
use utils::coordinates_hash::PointsHash;

/// Loads stl file, slices it at given thickness, mills all slices
/// and return global path.
pub fn compute_milling_path(thickness: f64, milling_radius: f64,
                            stl_file: String) {
    module_debug!({
        println!("Starting jimn, loading {}, thickness is {}, radius is {}",
                 stl_file, thickness, milling_radius
                 );
    });
    let mut model = Stl::new(&stl_file).expect("error loading stl file");
    let mut points_hash = PointsHash::new(2, 6);
    let slices = model.compute_slices(thickness, &mut points_hash);
    for (_, segments) in slices {
        assert!(!segments.is_empty());
        let polygons = build_polygons(segments);
        display!(polygons);
    }
}
