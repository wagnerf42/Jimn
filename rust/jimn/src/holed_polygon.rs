//! Holed Polygons.
use std::collections::HashMap;
use classifier::{HasEdge, build_inclusion_tree};
use polygon::Polygon;
use quadrant::{Quadrant, Shape};

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
        let mut strings: Vec<String> = self.holes
            .iter()
            .map(|h| h.svg_string())
            .collect();
        strings.push(self.polygon.svg_string());
        strings.join("\n")
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
    let included_polygons = build_inclusion_tree(polygons);
    let mut heights: HashMap<usize, u32> = HashMap::new();
    let mut holed_polygons: HashMap<usize, HoledPolygon> = HashMap::new();
    heights.insert(0, 0); // root node is at level 0
    for node in included_polygons.nodes.into_iter().skip(1) {
        let height = heights[&node.father.unwrap()] + 1;
        heights.insert(node.index, height);
        if height % 2 == 1 {
            holed_polygons.insert(node.index, HoledPolygon::new(node.value, Vec::new()));
        } else {
            holed_polygons.get_mut(&node.father.unwrap()).unwrap().add_hole(node.value);
        }
    }
    //TODO: report this clippy false positive
    let result = holed_polygons.drain().map(|(_, v)| v).collect();
    result
}
