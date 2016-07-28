//! **Jimn** is a geometry module intended for computing paths for CNC machines.
//!
//! It however can still be used as a standalone module for doing geometry.
//! You can find standard graphical objects xxx in sub-modules jimn::xxx like :
//!
//! * point
//! * segment
//! * arc
//! * polygon
//! * circle
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

pub mod bounding_box;
pub mod point;
pub mod segment;
pub mod stl;
pub mod tycat;
pub mod utils;
