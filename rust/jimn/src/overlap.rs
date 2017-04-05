//! Remove overlapping parts from a set of segments.
use std::collections::HashMap;
use segment::Segment;
use point::Point;
use utils::coordinates_hash::CoordinatesHash;

/// Take some segments and remove all overlapping parts.
pub fn remove_overlaps(segments: &[Segment]) -> Vec<Segment> {
    // Hash each point as starting or ending point on the line going through the segment.
    // We use line keys that we need to round to decrease the chance of precision problems.
    let mut key_hashes = [CoordinatesHash::new(5), CoordinatesHash::new(5)];
    let mut events = HashMap::new();
    for segment in segments {
        let key = segment.line_key();
        let adjusted_key = (key_hashes[0].hash_coordinate(key.0),
                            key_hashes[1].hash_coordinate(key.1));
        let line_entry = events.entry(adjusted_key).or_insert_with(HashMap::new);
        {
            let start_entry = line_entry.entry(segment.start).or_insert(0);
            *start_entry += 1;
        }
        let end_entry = line_entry.entry(segment.end).or_insert(0);
        *end_entry -= 1;
    }
    let mut results = Vec::new();
    /// Now follow points on each line to get what we want.
    for line_events in events.values() {
        play_line_events(line_events, &mut results);
    }
    results
}

fn play_line_events(events: &HashMap<Point, i32>, results: &mut Vec<Segment>) {
    let mut sorted_points: Vec<&Point> = events.keys().collect();
    sorted_points.sort();
    // count in how many segments we are
    let mut counter = 0;
    let mut entry_point: Option<&Point> = None; // start of segment being built
    for point in sorted_points {
        counter += events[point];
        if counter % 2 == 0 {
            // we are now in a part we must not keep
            if let Some(previous_point) = entry_point {
                // we are leaving a part we must keep
                results.push(Segment::new(*previous_point, *point));
                entry_point = None;
            }
        } else if entry_point.is_none() {
            // we are now entering a part we must keep
            entry_point = Some(point);
        }
    }
}
