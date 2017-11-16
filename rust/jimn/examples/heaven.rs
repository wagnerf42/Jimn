extern crate itertools;
#[macro_use]
extern crate jimn;
use itertools::Itertools;

use jimn::{Point, Polygon, Segment};
use jimn::utils::coordinates_hash::PointsHash;
use jimn::overlap::cut_overlaps;
use jimn::bentley_ottmann::bentley_ottmann;
use jimn::polygon::build_polygons;
use jimn::classifier::find_included_polygons;
use jimn::tree::Tree;



fn main() {
    let mut earth: Vec<_> = [
        Point::new(0.0, 0.0),
        Point::new(5.0, 0.0),
        Point::new(4.0, 3.0),
        Point::new(1.0, 5.0),
    ].iter()
        .cycle()
        .tuple_windows()
        .take(4)
        .map(|(a, b)| Segment::new(*a, *b))
        .collect();

    //    earth.extend(
    //        [
    //            Point::new(2.0, 3.0),
    //            Point::new(3.0, 3.0),
    //            Point::new(2.5, 3.5),
    //        ].iter()
    //            .cycle()
    //            .tuple_windows()
    //            .take(3)
    //            .map(|(a, b)| Segment::new(*a, *b)),
    //    );

    let mut heaven: Vec<_> = [
        Point::new(0.0, 2.0),
        Point::new(3.0, -1.0),
        Point::new(4.5, 1.5),
        Point::new(4.0, 3.0),
        Point::new(3.0, 2.0),
    ].iter()
        .cycle()
        .tuple_windows()
        .take(5)
        .map(|(a, b)| Segment::new(*a, *b))
        .collect();

    //    heaven.extend(
    //        [
    //            Point::new(3.0, 1.0),
    //            Point::new(4.0, 1.5),
    //            Point::new(3.5, 2.0),
    //        ].iter()
    //            .cycle()
    //            .tuple_windows()
    //            .take(3)
    //            .map(|(a, b)| Segment::new(*a, *b)),
    //    );

    display!(unicolor!(&earth), unicolor!(&heaven));

    let mut rounder = PointsHash::new(2);
    let earth: Vec<_> = earth
        .iter()
        .map(|s| {
            Segment::new(rounder.hash_point(&s.start), rounder.hash_point(&s.end))
        })
        .collect();
    let heaven: Vec<_> = heaven
        .iter()
        .map(|s| {
            Segment::new(rounder.hash_point(&s.start), rounder.hash_point(&s.end))
        })
        .collect();
    println!("heaven");
    display!(unicolor!(&heaven));

    println!("all paths");
    let mut all_paths = heaven.clone();
    all_paths.extend(earth);
    display!(multicolor!(&all_paths));

    println!("no overlaps");
    let non_overlapping = cut_overlaps(&all_paths);
    display!(multicolor!(&non_overlapping));

    println!("no intersections");
    let non_intersecting = bentley_ottmann(&non_overlapping, &mut rounder);
    display!(multicolor!(&non_intersecting));

    println!("polygons");
    let elementary_polygons = build_polygons(&non_intersecting);
    display!(multicolor!(&elementary_polygons));

    // classify elementary polygons in heaven and figure out which one is heavenly
    // classify elementary polygons in earth and figure out which one is earthly
    let heavenly_ids = find_included_polygons(&heaven, &elementary_polygons);
    println!("heavenly polygons");
    display!(multicolor!(
        elementary_polygons
            .iter()
            .zip(heavenly_ids.iter())
            .filter(|&(_, b)| *b)
            .map(|(p, _)| p)
    ));

    // now generate graphs by tiling and clipping each polygon
    // an earth polygon is completely filled
    // an heavenly polygon needs support generation
    // an earth + heaven polygon is sparcely filled
    // a polygon neither from earth nor heaven is from hell and discarded
}
