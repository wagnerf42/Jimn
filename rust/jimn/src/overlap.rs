//! Remove overlapping parts from a set of segments.
//! Two variants : remove them or cut segments into elementary paths on overlapping part.
use std::collections::HashMap;
use point::Point;
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::{BentleyOttmannPath, Cuttable, PathIndex};

fn keep_first_overlapping_path<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
    alive_paths: &[PathIndex],
    results: &mut Vec<T>,
    point: &Point,
    previous_point: &Point,
) {
    for alive_path in alive_paths.iter().take(1) {
        results.push(paths[*alive_path].new_from(point, previous_point));
    }
}

fn keep_overlapping_paths<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
    alive_paths: &[PathIndex],
    results: &mut Vec<T>,
    point: &Point,
    previous_point: &Point,
) {
    for alive_path in alive_paths {
        results.push(paths[*alive_path].new_from(point, previous_point));
    }
}

fn discard_overlapping_paths<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
    alive_paths: &[PathIndex],
    results: &mut Vec<T>,
    point: &Point,
    previous_point: &Point,
) {
    if alive_paths.len() < 2 {
        for alive_path in alive_paths {
            results.push(paths[*alive_path].new_from(point, previous_point));
        }
    }
}


/// Remove overlap in all segments.
/// pre-condition : no more than 2 segments can overlap.
pub fn remove_overlaps<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
) -> Vec<T> {
    handle_overlaps(paths, &discard_overlapping_paths)
}

/// Cut overlapping parts in all segments.
/// pre-condition : no more than 2 segments can overlap.
pub fn cut_overlaps<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(paths: &[T]) -> Vec<T> {
    handle_overlaps(paths, &keep_first_overlapping_path)
}

/// Take some paths segments and remove all overlapping parts on segments.
fn handle_overlaps<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
    cut_paths: &Fn(&[T], &[PathIndex], &mut Vec<T>, &Point, &Point),
) -> Vec<T> {
    // Hash each point as starting or ending point on the line going through the segment.
    // We use line keys that we need to round to decrease the chance of precision problems.
    let mut key_hash = PointsHash::new(5);
    let mut events = HashMap::new();
    let mut remaining_paths: Vec<T> = Vec::with_capacity(paths.len());

    for (index, path) in paths.iter().enumerate() {
        if let Some(key) = path.as_ref().path_key() {
            // we have a segment
            let adjusted_key = key_hash.hash_point(&key);

            let line_entry = events.entry(adjusted_key).or_insert_with(HashMap::new);
            line_entry
                .entry(path.as_ref().ordered_points().1)
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .0
                .push(index);
            line_entry
                .entry(path.as_ref().ordered_points().0)
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .1
                .push(index);
        } else {
            // we have an arc
            remaining_paths.push(*path);
        }
    }

    for line_events in events.values() {
        play_line_events(line_events, paths, &mut remaining_paths, cut_paths);
    }
    remaining_paths
}

fn play_line_events<P: BentleyOttmannPath, T: AsRef<P> + Cuttable>(
    events: &HashMap<Point, (Vec<PathIndex>, Vec<PathIndex>)>,
    paths: &[T],
    results: &mut Vec<T>,
    cut_paths: &Fn(&[T], &[PathIndex], &mut Vec<T>, &Point, &Point),
) {
    let mut sorted_points: Vec<&Point> = events.keys().collect();
    sorted_points.sort();
    let mut points = sorted_points.into_iter();
    let mut previous_point = points.next().unwrap();
    let mut alive_paths = events[&previous_point].0.clone();

    for point in points {
        cut_paths(paths, &alive_paths, results, &point, &previous_point);

        let ending_paths = &events[point].1;
        for ending_path in ending_paths {
            let index = alive_paths.iter().position(|p| p.eq(ending_path)).unwrap();
            alive_paths.remove(index);
        }
        previous_point = point;
        let starting_paths = &events[point].0;
        for starting_path in starting_paths {
            alive_paths.push(*starting_path);
        }
    }
}
