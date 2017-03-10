#[macro_use]
extern crate jimn;
use jimn::quadrant::{Quadrant, Shape};
use jimn::segment::save_segments;
use jimn::tycat::display;
use jimn::tile::hexagonal_tile;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::stl::Stl;

/// try a square tile on a triangle
fn main() {
    let mut rounder = PointsHash::new(6);
    let model = Stl::new("../../test_files/Carnifex.stl").expect("unable to load stl file");
    let slices = model.compute_slices(1.0, &mut rounder);
    let index = 22;

    let mut quadrant = Quadrant::new(2);
    for segment in &slices[index].1 {
        quadrant.add(&segment.start);
        quadrant.add(&segment.end);
    }
    let tile = hexagonal_tile(0.5, 0.5);
    let tiled_slice = tile.tile(&quadrant, &mut rounder);
    display!(slices[index].1, tiled_slice);

    let mut all = Vec::new();
    all.extend(tiled_slice);
    all.extend(slices[index].1.iter().cloned());
    save_segments("carnifex_h_0.5.bo", &all).expect("failed writing");
    bentley_ottmann(&all, &mut rounder);
}
