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
#![feature(plugin)]
#![plugin(clippy)]

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
