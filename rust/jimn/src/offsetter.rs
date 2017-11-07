//! offsetting related functions
use std::iter::repeat;
use {Arc, ElementaryPath, HoledPocket};
use polygon::Polygon;
use holed_polygon::HoledPolygon;
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::bentley_ottmann;
use pocket::build_pockets;
use classifier::complete_inclusion_tree;
use tree::Tree;
use overlap::remove_overlaps;


/// Add to given vector all paths obtained when taking inner parallel segments in a polygon
/// (displaced by radius) and looping around endpoints.
pub fn inner_paths(
    polygon: &Polygon,
    radius: f64,
    paths: &mut Vec<ElementaryPath>,
    rounder: &mut PointsHash,
) {
    let radius = radius.into();
    let mut segments = polygon.segments();
    let first_segment = segments.next().unwrap();
    let first_inner_segment =
        ElementaryPath::parallel_segment(&first_segment, radius, true, rounder);
    let start_point = *first_inner_segment.start();
    let start_center = first_segment.start;
    let mut previous_point = *first_inner_segment.end();
    paths.push(first_inner_segment);
    for segment in segments {
        let inner_segment = ElementaryPath::parallel_segment(&segment, radius, true, rounder);
        let arc = Arc::new(
            previous_point,
            *inner_segment.start(),
            segment.start,
            radius,
        );
        let sub_arcs = arc.split_for_unique_y(rounder);
        if let Some((a1, a2)) = sub_arcs {
            paths.push(ElementaryPath::Arc(a1));
            paths.push(ElementaryPath::Arc(a2));
        } else {
            paths.push(ElementaryPath::Arc(arc));
        }

        previous_point = *inner_segment.end();
        paths.push(inner_segment);
    }
    //add last arc
    let last_point = *paths.last().unwrap().end();
    paths.push(ElementaryPath::Arc(
        Arc::new(last_point, start_point, start_center, radius),
    ));
}

/// Offset given `HoledPolygon` at given distance.
/// Return a vector of `HoledPocket`.
pub fn offset_holed_polygon(
    holed_polygon: &HoledPolygon,
    radius: f64,
    rounder: &mut PointsHash,
) -> Vec<HoledPocket> {
    //TODO: take as input a vec of polygons to do all classify in one sweep
    let mut raw_paths = Vec::new();
    // take some segments parallel to holed poly, on the inside (joined with arcs)
    module_debug!({
        println!("starting offsetter on:");
        display!(holed_polygon);
    });
    for polygon in holed_polygon.polygons() {
        inner_paths(polygon, radius, &mut raw_paths, rounder);
    }
    module_debug!({
        println!("we computed offsetted paths:");
        display!(holed_polygon, unicolor!(&raw_paths));
    });

    // convert to elementary paths
    let paths_without_overlaps = remove_overlaps(&raw_paths);
    module_debug!({
        println!("after removing overlaps:");
        display!(holed_polygon, unicolor!(&paths_without_overlaps));
    });
    let small_paths = bentley_ottmann(&paths_without_overlaps, rounder);
    module_debug!({
        println!("after computing all intersections:");
        display!(holed_polygon, unicolor!(&small_paths));
    });

    // build a set of small pockets
    let pockets = build_pockets(&small_paths);
    module_debug!({
        println!("after rebuilding pockets");
        display!(multicolor!(&pockets));
    });

    // figure out which pocket is inside what
    let mut pockets_tree = Tree::new();
    complete_inclusion_tree(&mut pockets_tree, pockets);
    module_debug!({
        println!("pockets for the following inclusion tree:");
        pockets_tree.tycat().expect("pockets tree display failed");
    });

    // now all correctly oriented children of root are our results
    // with subnodes as the holes in each
    let mut holed_pockets = Vec::new();
    let mut indices: Vec<usize> = repeat(0).take(pockets_tree.nodes.len()).collect();
    for node in pockets_tree.nodes.drain(..) {
        if let Some(father) = node.father {
            if father == 0 {
                if node.value.is_oriented_clockwise() {
                    indices[node.index] = holed_pockets.len();
                    holed_pockets.push(HoledPocket::new(
                        node.value,
                        Vec::with_capacity(node.children.len()),
                    ));
                }
            } else {
                holed_pockets[indices[father]].add_hole(node.value);
            }
        }
    }
    module_debug!({
        println!("final result is:");
        display!(multicolor!(&holed_pockets));
    });
    holed_pockets
}
