//! inclusion tree builder
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use bentley_ottmann::{SegmentIndex, Key, KeyGenerator};
use point::Point;
use segment::Segment;
use tree::treap::{CountingTreap, KeyComputer};
use super::ClippingSegment;

type ClassifyEvent = (Point, Vec<SegmentIndex>, Vec<SegmentIndex>);
type CTreap<'a> = CountingTreap<SegmentIndex, Key, KeyGenerator<'a, ClippingSegment>>;
type Generator<'a> = Rc<RefCell<KeyGenerator<'a, ClippingSegment>>>;

/// Takes a set of segments (the clip) forming a polygon and another set of segments (the clipped).
/// Return all segments inside the clip.
/// pre-condition: no intersection.
pub fn classify_clip_segments(segments: &[ClippingSegment]) -> Vec<Segment> {

    let generator = KeyGenerator::new(segments);
    let mut crossed_clip_segments = CountingTreap::new(generator.clone());
    let events = create_events(segments);
    run_events(&events, generator, &mut crossed_clip_segments)
}

fn run_events(events: &[ClassifyEvent],
              generator: Generator,
              crossed_clip_segments: &mut CTreap)
              -> Vec<Segment> {
    let mut kept_segments = Vec::new();
    for event in events {
        end_segments(&event.2, &generator, crossed_clip_segments);
        generator.borrow_mut().current_point = event.0;
        start_segments(&event.1,
                       &generator,
                       crossed_clip_segments,
                       &mut kept_segments);
    }
    kept_segments
}

// Remove all clipping segments.
fn end_segments(ending: &[SegmentIndex],
                generator: &Generator,
                crossed_clip_segments: &mut CTreap) {
    for segment_index in ending {
        if generator.borrow().paths[*segment_index].clipping {
            crossed_clip_segments.remove(&generator.borrow().compute_key(segment_index));
        }
    }
}

// Start all clipping segments, categorize all others.
fn start_segments(starting: &[SegmentIndex],
                  generator: &Generator,
                  crossed_clip_segments: &mut CTreap,
                  kept_segments: &mut Vec<Segment>) {
    // we start by adding all clip segments
    for segment_index in starting {
        if generator.borrow().paths[*segment_index].clipping {
            crossed_clip_segments.add(*segment_index);
            // we keep the clipper in results
            kept_segments.push(generator.borrow().paths[*segment_index].segment);
        }
    }
    // now figure out wether we keep or not non-clip segments
    for segment_index in starting {
        if !generator.borrow().paths[*segment_index].clipping {
            let key = generator.borrow().compute_key(segment_index);
            //TODO: why find_key ?
            if crossed_clip_segments.find_key(&key).is_none() &&
               crossed_clip_segments.number_of_larger_nodes(&key) % 2 == 1 {
                kept_segments.push(generator.borrow().paths[*segment_index].segment);
            }
        }
    }
}

fn create_events(segments: &[ClippingSegment]) -> Vec<ClassifyEvent> {
    let mut raw_events = HashMap::with_capacity(2 * segments.len());
    for (index, segment) in segments.iter().enumerate() {
        let (first_point, last_point) = segment.as_ref().ordered_points();
        raw_events
            .entry(first_point)
            .or_insert_with(|| (Vec::new(), Vec::new()))
            .0
            .push(index);
        raw_events
            .entry(last_point)
            .or_insert_with(|| (Vec::new(), Vec::new()))
            .1
            .push(index);
    }

    let mut events: Vec<_> = raw_events
        .into_iter()
        .map(|(k, v)| (k, v.0, v.1))
        .collect();
    events.sort_by(|a, b| b.0.cmp(&a.0));
    events
}
