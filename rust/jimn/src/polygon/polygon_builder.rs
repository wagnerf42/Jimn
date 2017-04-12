use std::collections::{HashMap, HashSet};
use point::Point;
use segment::Segment;
use polygon::Polygon;
use ordered_float::NotNaN;

/// Converts segment into oriented polygons (clockwise) by following edges.
/// Flat polygons are discarded in the process.
pub fn build_polygons(segments: &[Segment]) -> Vec<Polygon> {
    let reversed_segments: Vec<_> = segments.iter().map(|s| s.reverse()).collect();
    let mut points = HashMap::new();
    let mut remaining_segments = HashSet::new();
    for segment in segments.iter().chain(reversed_segments.iter()) {
        points
            .entry(segment.start)
            .or_insert_with(Vec::new)
            .push(segment.end);
        remaining_segments.insert(segment);
    }
    for (point, neighbours) in &mut points {
        neighbours.sort_by(|p1, p2| point.angle_with(p1).cmp(&point.angle_with(p2)))
    }

    let mut polygons = Vec::new();
    while !remaining_segments.is_empty() {
        let next_start_segment = *remaining_segments.iter().next().unwrap();
        remaining_segments.remove(&next_start_segment);
        if let Some(polygon) = build_polygon(next_start_segment, &points, &mut remaining_segments) {
            polygons.push(polygon);
        }
    }
    polygons
}

/// Builds polygon obtained when following segment. Might return None if obtained polygon is flat.
fn build_polygon(start_segment: &Segment,
                 points: &HashMap<Point, Vec<Point>>,
                 remaining_segments: &mut HashSet<&Segment>)
                 -> Option<Polygon> {
    let starting_point = start_segment.start;
    let mut previous_point = starting_point;
    let mut current_point = start_segment.end;
    let mut polygon_points = vec![starting_point];
    remaining_segments.remove(start_segment);
    //follow edge until we come back to our starting point
    while current_point != starting_point {
        let next_point = find_next_point(&points[&current_point], &current_point, &previous_point);
        remaining_segments.remove(&Segment::new(current_point, next_point));
        polygon_points.push(current_point);
        previous_point = current_point;
        current_point = next_point;
    }
    let polygon = Polygon::new(polygon_points);
    let area = polygon.area();
    //TODO: check which orientation we really want and adjust increment in find next accordingly
    if area > NotNaN::new(-0.00001).unwrap() {
        // discard both flat and badly oriented polygons
        None
    } else {
        //keep only reverse-clockwise polygons
        Some(polygon.simplify())
    }
}

fn find_next_point(neighbours: &[Point], current_point: &Point, previous_point: &Point) -> Point {
    let incoming_angle = current_point.angle_with(previous_point);
    let index = neighbours
        .binary_search_by_key(&incoming_angle, |p| current_point.angle_with(p))
        .unwrap();
    neighbours[(index + 1) % neighbours.len()]
}
