use std::collections::{HashMap, HashSet};
use point::Point;
use elementary_path::ElementaryPath;
use pocket::Pocket;
use ordered_float::NotNaN;

/// Converts elementary paths into oriented pockets (clockwise) by following edges.
/// Flat pockets are discarded in the process.
pub fn build_pockets(paths: &[ElementaryPath]) -> Vec<Pocket> {
    let mut points = HashMap::new();
    let mut remaining_paths = HashSet::new();
    for path in paths {
        points
            .entry(path.start())
            .or_insert_with(Vec::new)
            .push(path);
        remaining_paths.insert(path);
    }
    //    for (point, neighbours) in &mut points {
    //        neighbours.sort_by(|p1, p2| point.angle_with(p1).cmp(&point.angle_with(p2)))
    //    }
    //
    //    let mut polygons = Vec::new();
    //    while !remaining_segments.is_empty() {
    //        let next_start_segment = *remaining_segments.iter().next().unwrap();
    //        remaining_segments.remove(&next_start_segment);
    //        if let Some(polygon) = build_polygon(next_start_segment, &points, &mut remaining_segments) {
    //            polygons.push(polygon);
    //        }
    //    }
    //    polygons
    unimplemented!()
}

/// Builds pocket obtained when following given start path. Might return None if obtained pocket is flat.
fn build_pocket(
    start_path: &ElementaryPath,
    points: &HashMap<Point, Vec<Point>>,
    remaining_paths: &mut HashSet<&ElementaryPath>,
) -> Option<Pocket> {
    unimplemented!()
    //    let starting_point = start_segment.start;
    //    let mut previous_point = starting_point;
    //    let mut current_point = start_segment.end;
    //    let mut polygon_points = vec![starting_point];
    //    remaining_segments.remove(start_segment);
    //    //follow edge until we come back to our starting point
    //    while current_point != starting_point {
    //        let next_point = find_next_point(&points[&current_point], &current_point, &previous_point);
    //        remaining_segments.remove(&Segment::new(current_point, next_point));
    //        polygon_points.push(current_point);
    //        previous_point = current_point;
    //        current_point = next_point;
    //    }
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
}

fn find_next_point(neighbours: &[Point], current_point: &Point, previous_point: &Point) -> Point {
    unimplemented!();
    //    let incoming_angle = current_point.angle_with(previous_point);
    //    let index = neighbours
    //        .binary_search_by_key(&incoming_angle, |p| current_point.angle_with(p))
    //        .unwrap();
    //    neighbours[(index + 1) % neighbours.len()]
}
