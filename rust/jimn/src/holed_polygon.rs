//! Holed Polygons.
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
