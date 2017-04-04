//! Remove overlapping parts from a set of segments.
use std::collections::HashMap;
use segment::Segment;
use point::Point;

/// Take some segments and remove all overlapping parts.
pub fn remove_overlaps(segments: &[Segment]) -> Vec<Segment> {
    let mut events = HashMap::new();
    for segment in segments {
        let key = segment.line_key();
        let line_entry = events.entry(key).or_insert_with(HashMap::new);
        {
            let start_entry = line_entry.entry(segment.start).or_insert(0);
            *start_entry += 1;
        }
        let end_entry = line_entry.entry(segment.end).or_insert(0);
        *end_entry -= 1;
    }
    let mut results = Vec::new();
    for line_events in events.values() {
        play_line_events(line_events, &mut results);
    }
    results
}

fn play_line_events(events: &HashMap<Point, usize>, results: &mut Vec<Segment>) {
    let mut sorted_points: Vec<&Point> = events.keys().collect();
    sorted_points.sort();
    // count in how many segments we are
    let mut counter = 0;
    let mut entry_point: Option<&Point> = None; // start of segment being built
    for point in sorted_points {
        counter += events[point];
        if counter % 2 == 1 {
            // we are now in a part we must not keep
            if let Some(previous_point) = entry_point {
                // we are leaving a part we must keep
                results.push(Segment::new(*previous_point, *point));
            }
        } else if entry_point.is_none() {
            // we are now entering a part we must keep
            entry_point = Some(point);
        }
    }
}
