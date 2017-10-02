use std::f64::consts::{FRAC_PI_2, PI};
use std::collections::{HashMap, HashSet};
use point::Point;
use elementary_path::ElementaryPath;
use pocket::Pocket;

type PathIndex = usize;
type Angle = f64;

/// Return couple of angles estimating where we go / come from.
/// Allows for ordering of connected paths.
fn angles(path: &ElementaryPath, point: &Point) -> (Angle, Angle) {
    let final_angle = point.angle_with(&path.other_endpoint(point));
    match *path {
        ElementaryPath::Segment(_) => (final_angle, final_angle),
        ElementaryPath::Arc(ref a) => {
            let mut tangent_angle = a.center.angle_with(point) + FRAC_PI_2;
            if ((final_angle - tangent_angle) % (PI * 2.0)).abs() > PI {
                tangent_angle -= PI;
            }
            (tangent_angle % (PI * 2.0), final_angle)
        }
    }
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
            .push((path_index, angles(&path, path.start()), 1));
        points
            .entry(path.end())
            .or_insert_with(Vec::new)
            .push((path_index, angles(&path, path.end()), -1));
        remaining_paths.insert(path_index);
    }

    for neighbours in points.values_mut() {
        neighbours.sort_by(|&(_, b, _), &(_, a, _)| {
            a.0
                .partial_cmp(&b.0)
                .unwrap()
                .then(a.1.partial_cmp(&b.1).unwrap())
        });
    }

    let mut pockets = Vec::new();
    while !remaining_paths.is_empty() {
        let next_start_path_index = *remaining_paths.iter().next().unwrap();
        if let Some(pocket) =
            build_pocket(next_start_path_index, paths, &points, &mut remaining_paths)
        {
            pockets.push(pocket);
        }
    }
    pockets
}

/// Builds pocket obtained when following given start path.
/// Might return None if obtained pocket is flat.
fn build_pocket(
    start_path_index: PathIndex,
    paths: &[ElementaryPath],
    points: &HashMap<&Point, Vec<(PathIndex, (Angle, Angle), i8)>>,
    remaining_paths: &mut HashSet<PathIndex>,
) -> Option<Pocket> {
    let mut current_path_index = start_path_index;
    let mut pocket_paths = Vec::new();
    //follow edge until we come back to our starting point
    while remaining_paths.take(&current_path_index).is_some() {
        let current_path = &paths[current_path_index];
        pocket_paths.push(*current_path);
        let current_point = current_path.end();
        current_path_index = find_next_path(&points[current_point], current_path);
    }
    Some(Pocket::new(pocket_paths))
}

/// Return where to go next when arriving from given path.
fn find_next_path(
    neighbours: &[(PathIndex, (Angle, Angle), i8)],
    current_path: &ElementaryPath,
) -> PathIndex {
    let incoming_angles = angles(current_path, current_path.end());
    // first figure out where we arrive
    let incoming_index = neighbours
        .binary_search_by(|&(_, a, _)| {
            incoming_angles
                .0
                .partial_cmp(&a.0)
                .unwrap()
                .then(incoming_angles.1.partial_cmp(&a.1).unwrap())
        })
        .unwrap();
    // now, rotate through neighbours until we find a leaving neighbouring path which is not
    // compensated by an incoming one.
    let mut count = 0;
    for &(index, _, path_type) in neighbours[(incoming_index + 1)..]
        .iter()
        .chain(neighbours[..incoming_index].iter())
    {
        count += path_type;
        if count == 1 {
            return index;
        }
    }
    panic!("we turned around and found no one");
}
