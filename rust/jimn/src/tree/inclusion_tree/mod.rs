//! Provides `InclusionTree` structure : which polygon is included
//! in which one.
//! First tree structure in use in milling algorithm.
use sweeping_lines::{SweepingLineAlgorithm, SweepingLineState};
use polygon::Polygon;
use tree::Tree;
use point::Point;
use tycat::{Displayable, display};
pub mod polygon_segment;

struct Builder {
    polygons: Vec<Polygon>
}

impl SweepingLineAlgorithm for Builder {
    fn add_path(&mut self, state: &mut SweepingLineState) {
        return;
    }
    fn remove_path(&mut self, state: &mut SweepingLineState) {
        return;
    }
}

/// Figures out which polygon is inside which other.
/// In case of equalities, height information is used (top contains bottom).
pub fn build_inclusion_tree(slices: Vec<(f64, Vec<Polygon>)>) {
    //-> Tree<(f64, Polygon)> {
    for (_, polygons) in slices {
        display!(polygons)
    }
}
