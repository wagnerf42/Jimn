use std::collections::{HashMap, HashSet};
use point::Point;
use elementary_path::ElementaryPath;
use pocket::Pocket;
use ordered_float::NotNaN;
type PathIndex = usize;

/// we need to order paths around each starting point.
/// we use the outgoing angle for that.
fn start_angle(path: &ElementaryPath) -> NotNaN<f64> {
    unimplemented!()
}

/// we arriving on a new point we need to figure out where to go next.
fn end_angle(path: &ElementaryPath) -> NotNaN<f64> {
    unimplemented!()
}

/// Converts elementary paths into oriented pockets (clockwise) by following edges.
/// Flat pockets are discarded in the process.
pub fn build_pockets(paths: &[ElementaryPath]) -> Vec<Pocket> {
    let mut points = HashMap::new();
    let mut remaining_paths = HashSet::new();
    for (path_index, path) in paths.iter().enumerate() {
        points
            .entry(path.start())
            .or_insert_with(Vec::new)
            .push(path_index);
        remaining_paths.insert(path_index);
    }

    for neighbours in points.values_mut() {
        neighbours.sort_by(|i1, i2| {
            start_angle(&paths[*i1]).cmp(&start_angle(&paths[*i2]))
        })
    }

    let mut pockets = Vec::new();
    while !remaining_paths.is_empty() {
        let next_start_path_index = *remaining_paths.iter().next().unwrap();
        remaining_paths.remove(&next_start_path_index);
        if let Some(pocket) =
            build_pocket(next_start_path_index, paths, &points, &mut remaining_paths)
        {
            pockets.push(pocket);
        }
    }
    pockets
}

/// Builds pocket obtained when following given start path. Might return None if obtained pocket is flat.
fn build_pocket(
    start_path_index: PathIndex,
    paths: &[ElementaryPath],
    points: &HashMap<&Point, Vec<PathIndex>>,
    remaining_paths: &mut HashSet<PathIndex>,
) -> Option<Pocket> {
    let mut current_path = paths[start_path_index];
    let starting_point = current_path.start();
    let mut previous_point = starting_point;
    //let mut current_point = current_path.end();
    let mut ending_angle = end_angle(&current_path);
    let mut pocket_paths = vec![current_path];
    remaining_paths.remove(&start_path_index);
    //follow edge until we come back to our starting point
    //while current_point != starting_point {
    //let next_point = find_next_path(&points[&current_point], ending_angle);
    //        remaining_segments.remove(&Segment::new(current_point, next_point));
    //        polygon_points.push(current_point);
    //        previous_point = current_point;
    //        current_point = next_point;
    //}
    //    let polygon = Polygon::new(polygon_points);
    //    let area = polygon.area();
    //    //TODO: check which orientation we really want and adjust increment in find next accordingly
    //    if area > NotNaN::new(-0.00001).unwrap() {
    //        // discard both flat and badly oriented polygons
    //        None
    //    } else {
    //        //keep only reverse-clockwise polygons
    //        Some(polygon.simplify())
    //    }
    unimplemented!()
}

fn find_next_path(neighbours: &[PathIndex], arriving_angle: NotNaN<f64>) -> PathIndex {
    unimplemented!();
    //    let incoming_angle = current_point.angle_with(previous_point);
    //    let index = neighbours
    //        .binary_search_by_key(&incoming_angle, |p| current_point.angle_with(p))
    //        .unwrap();
    //    neighbours[(index + 1) % neighbours.len()]
}
