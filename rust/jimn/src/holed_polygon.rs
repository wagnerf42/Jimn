//! Holed Polygons.
use ordered_float::NotNaN;
use std::collections::HashMap;
use classifier::{HasEdge, complete_inclusion_tree};
use segment::Segment;
use polygon::{Polygon, build_polygons};
use quadrant::{Quadrant, Shape};
use tree::Tree;

/// Polygon with some potential holes inside
pub struct HoledPolygon {
    polygon: Polygon,
    holes: Vec<Polygon>,
}

impl HoledPolygon {
    /// Create a new HoledPolygon.
    pub fn new(polygon: Polygon, holes: Vec<Polygon>) -> Self {
        HoledPolygon {
            polygon: polygon,
            holes: holes,
        }
    }

    /// Add given hole to ours.
    pub fn add_hole(&mut self, hole: Polygon) {
        self.holes.push(hole);
    }
}

impl Shape for HoledPolygon {
    fn get_quadrant(&self) -> Quadrant {
        self.polygon.get_quadrant()
    }

    fn svg_string(&self) -> String {
        let mut strings: Vec<String> = self.holes.iter().map(|h| h.svg_string()).collect();
        strings.push(self.polygon.svg_string());
        strings.join("\n")
    }
}

impl Default for HoledPolygon {
    fn default() -> Self {
        HoledPolygon {
            polygon: Default::default(),
            holes: Vec::new(),
        }
    }
}

impl HasEdge for HoledPolygon {
    fn edge(&self) -> &Polygon {
        &self.polygon
    }
}

/// Turn given Polygons into holed polygons.
#[cfg_attr(feature = "cargo-clippy", allow(let_and_return))] // disabled for false positive
pub fn build_holed_polygons(polygons: Vec<Polygon>) -> Vec<HoledPolygon> {
    let mut included_polygons = Tree::new();
    complete_inclusion_tree(&mut included_polygons, polygons);
    let mut heights: HashMap<usize, u32> = HashMap::new();
    let mut holed_polygons: HashMap<usize, HoledPolygon> = HashMap::new();
    heights.insert(0, 0); // root node is at level 0
    included_polygons.topological_renumbering();
    for node in included_polygons.nodes.into_iter().skip(1) {
        let height = heights[&node.father.unwrap()] + 1;
        heights.insert(node.index, height);
        if height % 2 == 1 {
            holed_polygons.insert(node.index, HoledPolygon::new(node.value, Vec::new()));
        } else {
            holed_polygons
                .get_mut(&node.father.unwrap())
                .unwrap()
                .add_hole(node.value);
        }
    }
    let result: Vec<HoledPolygon> = holed_polygons.drain().map(|(_, v)| v).collect();
    result
}

/// Turns slices of 3d model into holed polygon tree.
pub fn build_holed_polygons_tree(slices: &[(NotNaN<f64>, Vec<Segment>)]) -> Tree<HoledPolygon> {
    let mut holed_polygons_tree = Tree::new();
    // inclusion tree is built from top to bottom
    for slice in slices.iter().rev() {
        let segments = &slice.1;
        let polygons = build_polygons(segments);
        let holed_polygons = build_holed_polygons(polygons);
        let old_size = holed_polygons_tree.len();
        complete_inclusion_tree(&mut holed_polygons_tree, holed_polygons);
        // each newly added holed polygon must be set as a child of someone added before.
        // it is however possible to have holed polygons included one into another.
        // the classifier would therefore connect two new holed polygons together.
        // we then need to move them up in the tree towards the first ancestor which is of the
        // previous level.
        holed_polygons_tree.rebranch_upward(old_size);
    }
    holed_polygons_tree
}
