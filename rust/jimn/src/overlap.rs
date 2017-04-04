//! Remove overlapping parts from a set of segments.
use std::collections::HashMap;
use segment::Segment;
use point::Point;

type SegmentIndex = usize;

/// Take some segments and remove all overlapping parts.
pub fn remove_overlaps(segments: &[Segment]) -> Vec<Segment> {
    let mut events = HashMap::new();
    for (index, segment) in segments.iter().enumerate() {
        let key = segment.line_key();
        let entry = events.entry(key).or_insert_with(Vec::new);
        entry.push((segment.start, 0, index));
        entry.push((segment.end, 1, index));
    }
    let mut results = Vec::new();
    for line_events in events.values_mut() {
        play_line_events(segments, line_events, &mut results);
    }
    results
}

fn play_line_events(segments: &[Segment],
                    events: &mut Vec<(Point, u8, SegmentIndex)>,
                    results: &mut Vec<Segment>) {
    events.sort();
    for &mut (point, event_type, segments) in events {
        unimplemented!()
    }
    unimplemented!()
}
