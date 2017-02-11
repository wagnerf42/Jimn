//! **Jimn** is a geometry module intended for computing paths for CNC machines.
//!
//! It however can still be used as a standalone module for doing geometry.
//! You can find standard graphical objects xxx in sub-modules `jimn::xxx` like :
//!
//! * point
//!
//! Jimn is intended to work with the terminology terminal emulator from
//! enlightenment. Under terminology any object or list of objects can be
//! graphically displayed using the [display!](macro.display!.html) macro from `jimn::tycat`.
//! Many graphical examples are available in the *examples* sub-directory.
//! Compile them with cargo !

#![deny(missing_docs)]
#![allow(dead_code, unused_imports, unused_variables)]

extern crate byteorder;
extern crate rand;
extern crate ordered_float;
//#[macro_use]
pub mod utils;
pub mod tycat;
pub mod quadrant;
pub mod point;
pub mod segment;
pub mod stl;

use ordered_float::NotNaN;
use stl::Stl;
use utils::coordinates_hash::PointsHash;

/// Computes the milling path for given slices thickness, milling radius and stl file.
pub fn compute_milling_path(thickness: f64, milling_radius: f64, stl_file: String) {
    let model = Stl::new(stl_file.as_str()).expect("unable to load stl file");
    let mut rounder = PointsHash::new(6);
    let slices = model.compute_slices(NotNaN::new(thickness).unwrap(), &mut rounder);
}
