//! Remove overlapping parts from a set of segments.
//! Two variants : remove them or cut segments into elementary paths on overlapping part.
use std::collections::HashMap;
use point::Point;
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::{BentleyOttmannPath, Cuttable, PathIndex};

trait OverlapBehavior {
    fn do_something() -> bool;
}

struct KeepOverlap();
impl OverlapBehavior for KeepOverlap {
    fn do_something() -> bool {
        true
    }
}

struct DiscardOverlap();
impl OverlapBehavior for DiscardOverlap {
    fn do_something() -> bool {
        false
    }
}

/// Remove overlap in all segments.
/// pre-condition : no more than 2 segments can overlap.
pub fn remove_overlaps<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
) -> Vec<T> {
    handle_overlaps::<DiscardOverlap, P, T>(paths)
}

/// Cut overlapping parts in all segments.
/// pre-condition : no more than 2 segments can overlap.
pub fn cut_overlaps<P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(paths: &[T]) -> Vec<T> {
    handle_overlaps::<KeepOverlap, P, T>(paths)
}

/// Take some paths segments and remove all overlapping parts on segments.
fn handle_overlaps<O: OverlapBehavior, P: BentleyOttmannPath, T: AsRef<P> + Cuttable + Copy>(
    paths: &[T],
) -> Vec<T> {
    // Hash each point as starting or ending point on the line going through the segment.
    // We use line keys that we need to round to decrease the chance of precision problems.
    let mut key_hash = PointsHash::new(6);
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
        play_line_events::<O, P, T>(line_events, paths, &mut remaining_paths);
    }
    remaining_paths
}

fn play_line_events<O: OverlapBehavior, P: BentleyOttmannPath, T: AsRef<P> + Cuttable>(
    events: &HashMap<Point, (Vec<PathIndex>, Vec<PathIndex>)>,
    paths: &[T],
    results: &mut Vec<T>,
) {
    let mut sorted_points: Vec<&Point> = events.keys().collect();
    sorted_points.sort();
    let mut entry_point: Option<&Point> = None; // start of segment being built
    let mut alive_paths: Vec<PathIndex> = Vec::with_capacity(2);

    for point in sorted_points {
        let ending_points = &events[point].1;
        for ending_path in ending_points {
            if alive_paths.len() - ending_points.len() != 1 || O::do_something() {
                if let Some(previous_point) = entry_point {
                    results.push(paths[*ending_path].new_from(point, previous_point));
                }
            }
            let index = alive_paths.iter().position(|p| p.eq(ending_path)).unwrap();
            alive_paths.remove(index);
        }
        entry_point = if alive_paths.len() == 1 {
            Some(point)
        } else {
            None
        };
        let starting_paths = &events[point].0;
        if !starting_paths.is_empty() {
            if let Some(previous_point) = entry_point {
                for alive_path in &alive_paths {
                    results.push(paths[*alive_path].new_from(point, previous_point));
                }
            }
        }
        for starting_path in starting_paths {
            alive_paths.push(*starting_path);
        }
        if alive_paths.len() == 1 || (alive_paths.len() == 2 && O::do_something()) {
            entry_point = Some(point);
        }
    }
}
