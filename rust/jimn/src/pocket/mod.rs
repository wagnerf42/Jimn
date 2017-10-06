//! `Pocket` class.
use std::f64::consts::PI;
use std::iter::once;
use {ElementaryPath, Quadrant};
use quadrant::Shape;

pub use self::pocket_builder::build_pockets;
mod pocket_builder;

/// `Polygon` equivalent, but also allowing arcs.
#[derive(Debug)]
pub struct Pocket {
    /// All paths forming the pocket, one after the other.
    pub edge: Vec<ElementaryPath>,
}

impl Default for Pocket {
    fn default() -> Self {
        Pocket { edge: Vec::new() }
    }
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
        if let Some(first_path) = self.edge.first() {
            let starting_point = first_path.start();
            once(format!(
                "<path d=\"M{},{}",
                starting_point.x,
                starting_point.y
            )).chain(self.edge.iter().map(|p| match *p {
                ElementaryPath::Segment(ref s) => format!(" L {} {}", s.end.x, s.end.y),
                ElementaryPath::Arc(ref a) => {
                    let sweep_flag = if a.angle() > PI { 1 } else { 0 };
                    format!(
                        " A {},{} 0 0,{} {},{}",
                        a.radius,
                        a.radius,
                        sweep_flag,
                        a.end.x,
                        a.end.y
                    )
                }
            }))
                .chain(once("\"/>".to_string()))
                .collect()
        } else {
            String::new()
        }
    }
}
