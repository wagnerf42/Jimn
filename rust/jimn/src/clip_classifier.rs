//! inclusion tree builder
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use bentley_ottmann::{SegmentIndex, Key, KeyGenerator};
use point::Point;
use segment::Segment;
use tree::treap::{CountingTreap, KeyComputer};

type ClassifyEvent = (Point, Vec<SegmentIndex>, Vec<SegmentIndex>);
type CTreap<'a> = CountingTreap<SegmentIndex, Key, KeyGenerator<'a, Segment>>;
type Generator<'a> = Rc<RefCell<KeyGenerator<'a, Segment>>>;

/// Takes a set of segments (the clip) forming a polygon and another set of segments (the clipped).
/// Return all segments inside the clip.
/// pre-condition: no intersection.
pub fn clip_segments(clip: Vec<Segment>, mut segments: Vec<Segment>) -> Vec<Segment> {
    let clip_index = segments.len(); // any index < clip_index is to be clipped
    segments.extend(clip);

    let generator = KeyGenerator::new(&segments);
    let mut crossed_clip_segments = CountingTreap::new(generator.clone());
    let events = create_events(&segments);
    run_events(&events, generator, &mut crossed_clip_segments, clip_index)
}

fn run_events(events: &[ClassifyEvent],
              generator: Generator,
              crossed_clip_segments: &mut CTreap,
              clip_index: usize)
              -> Vec<Segment> {
    let mut kept_segments = Vec::new();
    for event in events {
        end_segments(&event.2, crossed_clip_segments, clip_index);
        generator.borrow_mut().current_point = event.0;
        start_segments(&event.1,
                       &generator,
                       crossed_clip_segments,
                       clip_index,
                       &mut kept_segments);
    }
    kept_segments
}

fn end_segments(ending: &[SegmentIndex], crossed_clip_segments: &mut CTreap, clip_index: usize) {
    for segment_index in ending {
        if *segment_index >= clip_index {
            // we have a clip segment
            crossed_clip_segments.find_node(*segment_index).unwrap().remove();
        }
    }
}

fn start_segments(starting: &[SegmentIndex],
                  generator: &Generator,
                  crossed_clip_segments: &mut CTreap,
                  clip_index: usize,
                  kept_segments: &mut Vec<Segment>) {
    // we start by adding all clip segments
    for segment_index in starting {
        if *segment_index >= clip_index {
            crossed_clip_segments.add(*segment_index);
        }
    }
    // now figure out wether we keep or not non-clip segments
    // TODO: attention a >= : verifier comment placer les clip par rapport aux normaux
    for segment_index in starting {
        if *segment_index < clip_index {
            let key = generator.borrow().compute_key(segment_index);
            let larger_segments_number = crossed_clip_segments.number_of_larger_nodes(&key);
            if larger_segments_number % 2 == 1 {
                // we are inside the clip !
                kept_segments.push(generator.borrow().segments[*segment_index]);
            }
        }
    }
}

fn create_events(segments: &[Segment]) -> Vec<ClassifyEvent> {
    let mut raw_events = HashMap::with_capacity(2 * segments.len());
    for (index, segment) in segments.iter().enumerate() {
        let (first_point, last_point) = segment.ordered_points();
        raw_events.entry(first_point)
            .or_insert_with(|| (Vec::new(), Vec::new()))
            .0
            .push(index);
        raw_events.entry(last_point)
            .or_insert_with(|| (Vec::new(), Vec::new()))
            .1
            .push(index);
    }

    let mut events: Vec<_> = raw_events.into_iter().map(|(k, v)| (k, v.0, v.1)).collect();
    events.sort_by(|a, b| b.0.cmp(&a.0));
    events
}
