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
#![feature(conservative_impl_trait)]
#![feature(specialization, core_intrinsics)]
// allow easy returning of iterators
#![deny(missing_docs)]
#![cfg_attr(feature = "cargo-clippy", allow(float_cmp))]

extern crate byteorder;
extern crate disjoint_sets;
extern crate dyntreap;
extern crate float_cmp;
extern crate fnv;
#[macro_use]
extern crate itertools;
extern crate num_traits;
extern crate rayon;
#[cfg(test)]
extern crate test;

#[macro_use]
pub mod utils;
#[macro_use]
pub mod tycat;
pub mod quadrant;
pub use quadrant::Quadrant;
pub mod point;
pub use point::Point;
pub mod stl;
pub mod tree;
pub mod bentley_ottmann;
pub mod classifier;
pub mod clipper;
pub mod polygon;
pub use polygon::Polygon;
pub mod holed_polygon;
pub use holed_polygon::HoledPolygon;
pub mod tile;
pub use tile::Tile;
pub mod overlap;
pub mod segment;
pub use segment::Segment;
mod arc;
pub use arc::Arc;
pub mod elementary_path;
pub use elementary_path::ElementaryPath;
pub mod pocket;
pub use pocket::Pocket;
pub mod holed_pocket;
pub use holed_pocket::HoledPocket;
pub mod offsetter;
pub mod graph;
pub mod tagged_path;
pub use tagged_path::TaggedPath;

use itertools::Itertools;
use stl::Stl;
use utils::coordinates_hash::PointsHash;
use overlap::remove_overlaps;
use polygon::build_polygons;
use holed_polygon::build_holed_polygons;

/// Computes the milling path for given slices thickness, milling radius and stl file.
pub fn compute_milling_path(thickness: f64, _milling_radius: f64, stl_file: &str) {
    let model = Stl::new(stl_file).expect("unable to load stl file");
    let mut rounder = PointsHash::new(6);
    let slices = model.compute_slices(thickness, &mut rounder);
    for slice in &slices {
        display!(unicolor!(&slice.1));
    }
    let ceilings_slices: Vec<(f64, Vec<Segment>)> = slices
        .iter()
        .tuple_windows()
        .map(|(s1, s2)| {
            (
                s1.0,
                s1.1.iter().cloned().chain(s2.1.iter().cloned()).collect(),
            )
        })
        .collect();

    let segments: Vec<_> = slices
        .iter()
        .skip(1)
        .zip(ceilings_slices.iter())
        .map(|(slice, ceiling)| (remove_overlaps(&slice.1), remove_overlaps(&ceiling.1)))
        .collect();

    for &(ref to_fill, ref to_ceil) in &segments {
        println!("new slice");
        display!(unicolor!(to_fill));
        display!(unicolor!(to_ceil));
    }
}
