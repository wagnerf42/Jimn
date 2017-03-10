//! **Jimn** is a geometry module intended for computing paths for CNC machines.
//!
//! It however can still be used as a standalone module for doing geometry.
//! You can find standard graphical objects xxx in sub-modules `jimn::xxx` like :
//!
//! * point
//! * segment
//! * polygon
//!
//! Jimn is intended to work with the terminology terminal emulator from
//! enlightenment. Under terminology any object or list of objects can be
//! graphically displayed using the [display!](macro.display!.html) macro from `jimn::tycat`.
//! Many graphical examples are available in the *examples* sub-directory.
//! Compile them with cargo !

#![feature(test)]
#![deny(missing_docs)]
//#![allow(dead_code, unused_imports, unused_variables)]

extern crate byteorder;
extern crate rand;
extern crate ordered_float;
extern crate test;
extern crate ego_tree;

pub mod utils;
#[macro_use]
pub mod tycat;
pub mod quadrant;
pub mod point;
pub mod segment;
pub mod stl;
pub mod tree;
pub mod bentley_ottmann;
pub mod polygon;
pub mod tile;

use stl::Stl;
use utils::coordinates_hash::PointsHash;
use quadrant::{Quadrant, Shape};
use tycat::display;

/// Computes the milling path for given slices thickness, milling radius and stl file.
pub fn compute_milling_path(thickness: f64, _milling_radius: f64, stl_file: String) {
    //TODO: use asref or borrow
    let model = Stl::new(stl_file.as_str()).expect("unable to load stl file");
    let mut rounder = PointsHash::new(6);
    let slices = model.compute_slices(thickness, &mut rounder);
    for slice in &slices {
        println!("height is {}", slice.0);
        display!(slice.1);
    }
}
