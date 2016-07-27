//! **Jimn** is a geometry module intended for computing paths for CNC machines.
//!
//! It however can still be used as a standalone module for doing geometry.
//! You can find standard graphical objects xxx in sub-modules jimn.xxx like :
//!
//! * point
//! * segment
//! * arc
//! * polygon
//! * circle
//! * facet (3D)
//! * holed_polygon
//!
//! Jimn is intended to work with the terminology terminal emulator from
//! enlightenment. Under terminology any object or list of objects can be
//! graphically displayed using the [display](tycat/fn.display.html) function from jimn::tycat.
//! Many graphical examples are available in the *examples* sub-directory.
//! Compile them with cargo !

// see https://www.reddit.com/r/rust/comments/3fg0xr/how_do_i_find_the_max_value_in_a_vecf64/
//TODO: types iterables avec flottants au lieu de vecteurs ?

#![deny(missing_docs)]

/// Returns min in given vector of f64.
pub fn float_min(vector: &Vec<f64>) -> f64 {
    let mut current_min = vector[0];
    for value in vector {
        if *value < current_min {
            current_min = *value;
        }
    }
    return current_min;
}

/// Returns max in given vector of f64.
pub fn float_max(vector: &Vec<f64>) -> f64 {
    let mut current_max = vector[0];
    for value in vector {
        if *value > current_max {
            current_max = *value;
        }
    }
    return current_max;
}

pub mod bounding_box;
pub mod point;
pub mod point3;
pub mod segment;
pub mod facet;
pub mod stl;
pub mod tycat;
pub mod precision;
pub mod utils;
