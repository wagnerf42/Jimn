//! `Pocket` class.
use {ElementaryPath, Quadrant};
use quadrant::Shape;

pub use self::pocket_builder::build_pockets;
mod pocket_builder;

/// `Polygon` equivalent, but also allowing arcs.
#[derive(Debug)]
pub struct Pocket {
    edge: Vec<ElementaryPath>,
}

impl Pocket {
    /// Build a new `Pocket` from given paths forming its edge.
    pub fn new(edge: Vec<ElementaryPath>) -> Self {
        Pocket { edge }
    }
}

impl Shape for Pocket {
    fn get_quadrant(&self) -> Quadrant {
        let mut quadrant = Quadrant::new(2);
        for path in &self.edge {
            quadrant.update(&path.get_quadrant());
        }
        quadrant
    }
    fn svg_string(&self) -> String {
        let strings: Vec<_> = self.edge.iter().map(|p| p.start().to_string()).collect();
        let string = strings.join(" ");
        format!("<polygon points=\"{}\"/>", string)
    }
}
