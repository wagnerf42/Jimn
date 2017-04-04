//! **Jimn** is a geometry module intended for computing paths for CNC machines.
//!
//! It however can still be used as a standalone module for doing geometry.
//! You can find standard graphical objects xxx in sub-modules `jimn::xxx` like :
//!
//! * point
//! * segment
//! * polygon
//! * holed polygon
//!
//! Jimn is intended to work with the terminology terminal emulator from
//! enlightenment. Under terminology any object or list of objects can be
//! graphically displayed using the [display!](macro.display!.html) macro from `jimn::tycat`.
//! Many graphical examples are available in the *examples* sub-directory.
//! Compile them with cargo !

#![feature(test)]
#![feature(conservative_impl_trait)] // allow easy returning of iterators
#![deny(missing_docs)]

extern crate byteorder;
extern crate rand;
extern crate ordered_float;
extern crate test;

pub mod utils;
#[macro_use]
pub mod tycat;
pub mod quadrant;
pub mod point;
pub mod segment;
pub mod stl;
pub mod tree;
pub mod bentley_ottmann;
pub mod classifier;
pub mod clipper;
pub mod polygon;
pub mod holed_polygon;
pub mod tile;

use stl::Stl;
use utils::coordinates_hash::PointsHash;
use holed_polygon::build_holed_polygons_tree;

/// Computes the milling path for given slices thickness, milling radius and stl file.
pub fn compute_milling_path(thickness: f64, milling_radius: f64, stl_file: &str) {
    //TODO: use asref or borrow
    let mut model = Stl::new(stl_file).expect("unable to load stl file");
    let mut rounder = PointsHash::new(6);
    let mut slices = model.compute_slices(thickness, &mut rounder);
    // add the border around each slice
    model.dimensions.inflate(milling_radius * 3.0); // 3 for now
    for slice in &mut slices {
        slice.1.extend(model.dimensions.segments());
    }
    let holed_polygons = build_holed_polygons_tree(&slices);
    holed_polygons.tycat().expect("failed displaying holed polygons tree");
}
