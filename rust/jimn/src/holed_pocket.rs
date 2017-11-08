//! Holed Pockets.
use std::iter::once;
use {ElementaryPath, Pocket, Quadrant};
use quadrant::Shape;

#[derive(Debug)]
/// Pocket with some potential holes inside
pub struct HoledPocket {
    /// Outer pocket
    pub pocket: Pocket,
    /// Inner holes
    pub holes: Vec<Pocket>,
}

impl HoledPocket {
    /// Create a new holed pocket out of given parts.
    pub fn new(pocket: Pocket, holes: Vec<Pocket>) -> Self {
        HoledPocket { pocket, holes }
    }
    /// Add given hole to pocket.
    pub fn add_hole(&mut self, hole: Pocket) {
        self.holes.push(hole);
    }
    /// Iterate on all paths (including holes)
    pub fn paths<'a>(&'a self) -> impl Iterator<Item = &ElementaryPath> + 'a {
        once(&self.pocket)
            .chain(self.holes.iter())
            .flat_map(|h| h.edge.iter())
    }
}

// for now we just display the outer one.
impl Shape for HoledPocket {
    fn get_quadrant(&self) -> Quadrant {
        self.pocket.get_quadrant()
    }
    fn svg_string(&self) -> String {
        self.pocket.svg_string()
    }
}
