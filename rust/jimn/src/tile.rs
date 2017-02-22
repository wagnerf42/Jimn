//! `Tiles` structures handling different tiling patterns.
use ordered_float::NotNaN;
use quadrant::{Quadrant, Shape};
use point::Point;
use segment::Segment;

/// should we tile using hexagons or rectangles ?
enum TileType {
    Hexagon,
    Rectangle,
}

/// a `Tile` structure stores a tileable pattern
pub struct Tile {
    /// segments defining pattern (they do not overlap when tiled)
    segments: Vec<Segment>,
    /// should we tile hexagonally or rectangularly ?
    tile_type: TileType,
    /// quadrant around tile
    quadrant: Quadrant,
}

impl Tile {
    fn new(segments: Vec<Segment>, tile_type: TileType) -> Tile {
        let mut quadrant = Quadrant::new(2);
        for segment in &segments {
            quadrant.update(&segment.get_quadrant());
        }
        Tile {
            segments: segments,
            tile_type: tile_type,
            quadrant: quadrant,
        }
    }

    /// Tile ourselves on target quadrant.
    pub fn tile(&self, target: &Quadrant) -> Vec<Segment> {
        self.rectangular_tile(target)
    }

    /// Tile ourselves on target quadrant.
    /// TODO: give an offset.
    fn rectangular_tile(&self, target: &Quadrant) -> Vec<Segment> {
        let (dimensions, tile_dimensions) = (target.dimensions(), self.quadrant.dimensions());
        let needed_tiles: Vec<usize> = dimensions.iter()
            .zip(tile_dimensions.iter())
            .map(|(&d, &td)| (d / td).ceil() as usize)
            .collect();

        let extra_space = [tile_dimensions[0] * (needed_tiles[0] as f64) - dimensions[0],
                           tile_dimensions[1] * (needed_tiles[1] as f64) - dimensions[1]];

        let segments: Vec<Segment> = Vec::with_capacity(needed_tiles[0] * needed_tiles[1] *
                                                        self.segments.len());

        for i in 0..needed_tiles[0] {
            for j in 0..needed_tiles[1] {
                println!("{} {}", i, j);
            }
        }

        panic!("TODO");
    }
}

/// Return a rectangular `Tile` of given sizes.
pub fn rectangular_tile<T: Into<NotNaN<f64>>, U: Into<NotNaN<f64>>>(width: T, height: U) -> Tile {
    let origin = Point::new(0.0, 0.0);
    Tile::new(vec![Segment::new(origin, Point::new(width, 0.0)),
                   Segment::new(origin, Point::new(0.0, height))],
              TileType::Hexagon)
}
