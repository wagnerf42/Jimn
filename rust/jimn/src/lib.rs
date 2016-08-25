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

// see https://www.reddit.com/r/rust/comments/3fg0xr/how_do_i_find_the_max_value_in_a_vecf64/
//TODO: types iterables avec flottants au lieu de vecteurs ?

#![deny(missing_docs)]

extern crate byteorder;
#[macro_use]
pub mod tycat;
pub mod utils;
pub mod elementary_path;
pub mod bounding_box;
pub mod point;
pub mod pocket;
pub mod segment;
pub mod arc;
pub mod stl;
pub mod polygon;

use stl::Stl;
use tycat::{Displayable, display};

/// Loads stl file, slices it at given thickness, mills all slices
/// and return global path.
pub fn compute_milling_path(thickness: f64, milling_radius: f64,
                            stl_file: String) {
    module_debug!({
        println!("Starting jimn, loading {}, thickness is {}, radius is {}",
                 stl_file, thickness, milling_radius
                 );
    });
    let model = Stl::new(&stl_file).expect("error loading stl file");
    let slices = model.compute_slices(thickness);
    for (_, segments) in slices {
        display!(segments);
    }
}
