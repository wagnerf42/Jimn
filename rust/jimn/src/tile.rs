//! `Tiles` structures handling different tiling patterns.
use std::f64::consts::PI;
use ordered_float::NotNaN;
use quadrant::Quadrant;
use point::Point;
use segment::Segment;
use utils::coordinates_hash::PointsHash;

/// a `Tile` structure stores a tileable pattern
pub struct Tile {
    /// segments defining pattern (they do not overlap when tiled)
    segments: Vec<Segment>,
    /// by how much do we move when tiling horizontally ?
    horizontal_move: NotNaN<f64>,
    /// by how much do we move when going vertically ?
    vertical_move: NotNaN<f64>,
    /// by how much do we move horizontally at start of new lines ?
    horizontal_offset: NotNaN<f64>,
}

impl Tile {
    /// Tile ourselves on target quadrant, adjusting new points using rounder.
    pub fn tile(&self, target: &Quadrant, rounder: &mut PointsHash) -> Vec<Segment> {
        let (left_x, right_x) = target.limits(0);
        let (top_y, bottom_y) = target.limits(1);
        let mut result = Vec::new();
        let mut offseting = 0.0;
        let height = bottom_y - top_y + self.vertical_move;
        let steps = (height / self.vertical_move).ceil() as usize;
        for step in 0..steps {
            let current_x = left_x + self.horizontal_offset * offseting;
            let current_y = top_y + self.vertical_move * (step as f64);
            self.tile_line(rounder, current_x, current_y, right_x, &mut result);
            offseting = 1.0 - offseting;
        }
        result
    }

    /// Tile one line with ourselves, starting from given coordinates.
    fn tile_line(&self,
                 rounder: &mut PointsHash,
                 current_x: NotNaN<f64>,
                 current_y: NotNaN<f64>,
                 right_x: NotNaN<f64>,
                 result: &mut Vec<Segment>) {
        let width = right_x + self.horizontal_move - current_x;
        let steps = (width / self.horizontal_move).ceil() as usize;
        for step in 0..steps {
            let x = current_x + self.horizontal_move * (step as f64);
            let translation = Point::new(x, current_y);
            result.extend(self.segments.iter().map(|s| s.translate(&translation, rounder)));
        }
    }
}

/// Return a rectangular tile of given dimensions.
pub fn rectangular_tile<T: Into<NotNaN<f64>>, U: Into<NotNaN<f64>>>(width: T, height: U) -> Tile {
    let w = width.into();
    let h = height.into();
    let p1 = Point::new(0.0, 0.0);
    let p2 = Point::new(-w, 0.0);
    let p3 = Point::new(0.0, -h);
    Tile {
        segments: vec![Segment::new(p1, p2), Segment::new(p1, p3)],
        horizontal_move: w,
        vertical_move: h,
        horizontal_offset: NotNaN::new(0.0).unwrap(),
    }
}

/// Return a tile for brick like patterns of given dimensions.
pub fn brick_tile<T: Into<NotNaN<f64>>, U: Into<NotNaN<f64>>>(width: T, height: U) -> Tile {
    let mut tile = rectangular_tile(width, height);
    tile.horizontal_offset = tile.horizontal_move / 2.0;
    tile
}

/// Return a hexagonal tile (scaled) of given dimensions.
pub fn hexagonal_tile<T: Into<NotNaN<f64>>, U: Into<NotNaN<f64>>>(width: T, height: U) -> Tile {
    let w = width.into();
    let h = height.into();
    //segments are top half of hexagon with bottom right point at (0,0)
    let angle = PI / 3.0;
    let x = w / 2.0 * angle.cos();
    let y = h / 2.0 * angle.sin();
    let p1 = Point::new(0.0, 0.0);
    let p2 = Point::new(x - w / 2.0, -y);
    let p3 = Point::new(-x - w / 2.0, -y);
    let p4 = Point::new(-w, 0.0);
    Tile {
        segments: vec![Segment::new(p1, p2), Segment::new(p2, p3), Segment::new(p3, p4)],
        horizontal_move: w + x * 2.0,
        vertical_move: y,
        horizontal_offset: w / 2.0 + x,
    }
}
