//! 2-levels clip classifier.
//! keep paths inside some others.
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::Bound::*;

use bentley_ottmann::{BentleyOttmannPath, HasX, KeyGenerator, PathIndex, YCoordinate};
use dyntreap::CTreap;
use super::ClippingPath;

type ClassifyEvent = (YCoordinate, Vec<PathIndex>, Vec<PathIndex>);
type Generator<'a, K, P> = Rc<RefCell<KeyGenerator<'a, K, P, ClippingPath<P>>>>;

/// Takes a set of segments (the clip) forming a polygon and another set of segments (the clipped).
/// Return all segments inside the clip.
/// pre-condition: no intersection.
pub fn classify_clip_paths<
    K: Ord + HasX + Copy,
    P: Copy + BentleyOttmannPath<BentleyOttmannKey = K>,
>(
    paths: &[ClippingPath<P>],
) -> (Vec<P>, Vec<P>) {
    let generator = KeyGenerator::new(paths);
    let closure_generator = Rc::clone(&generator);
    let mut crossed_clip_paths =
        CTreap::new_with_key_generator(move |index| closure_generator.borrow().compute_key(index));
    let (events, horizontal_paths) = create_events(paths);
    run_events(
        &events,
        &generator,
        &mut crossed_clip_paths,
        &horizontal_paths,
    )
}

fn run_events<K: Ord + HasX + Copy, P: Copy + BentleyOttmannPath<BentleyOttmannKey = K>>(
    events: &[ClassifyEvent],
    generator: &Generator<K, P>,
    crossed_clip_paths: &mut CTreap<K, PathIndex>,
    horizontal_paths: &HashMap<YCoordinate, Vec<PathIndex>>,
) -> (Vec<P>, Vec<P>) {
    let mut clipped_paths = Vec::new();
    let mut clipping_paths = Vec::new();
    for event in events {
        end_paths(&event.2, generator, crossed_clip_paths, &mut clipping_paths);
        generator.borrow_mut().current_y = event.0;
        start_paths(&event.1, generator, crossed_clip_paths, &mut clipped_paths);
        // handle horizontal segments
        if let Some(paths) = horizontal_paths.get(&event.0) {
            for path_index in paths {
                if generator.borrow().paths[*path_index].clipping {
                    // we automatically keep clipping paths
                    clipping_paths.push(generator.borrow().paths[*path_index].path);
                } else {
                    let big_point = generator.borrow().paths[*path_index]
                        .as_ref()
                        .ordered_points()
                        .0;
                    let big_key = K::min_key(big_point.x); // min because excluded from bound
                    if crossed_clip_paths
                        .ordered_nodes((Excluded(big_key), Unbounded))
                        .count() % 2 == 1
                    {
                        clipped_paths.push(generator.borrow().paths[*path_index].path);
                    }
                }
            }
        }
    }
    (clipped_paths, clipping_paths)
}

// Remove all clipping segments.
fn end_paths<K: Ord + HasX + Copy, P: BentleyOttmannPath<BentleyOttmannKey = K> + Copy>(
    ending: &[PathIndex],
    generator: &Generator<K, P>,
    crossed_clip_paths: &mut CTreap<K, PathIndex>,
    clipping_paths: &mut Vec<P>,
) {
    for path_index in ending {
        if generator.borrow().paths[*path_index].clipping {
            crossed_clip_paths.remove(&generator.borrow().compute_key(path_index));
            clipping_paths.push(generator.borrow().paths[*path_index].path);
        }
    }
}

// Start all clipping paths, categorize all others.
fn start_paths<K: Ord + HasX + Copy, P: BentleyOttmannPath<BentleyOttmannKey = K> + Copy>(
    starting: &[PathIndex],
    generator: &Generator<K, P>,
    crossed_clip_paths: &mut CTreap<K, PathIndex>,
    kept_paths: &mut Vec<P>,
) {
    // we start by adding all clip segments
    for path_index in starting {
        if generator.borrow().paths[*path_index].clipping {
            crossed_clip_paths.insert(*path_index);
        }
    }
    // now figure out wether we keep or not non-clip segments
    for path_index in starting {
        if !generator.borrow().paths[*path_index].clipping {
            let key = generator.borrow().compute_key(path_index);
            if crossed_clip_paths
                .ordered_nodes((Excluded(key), Unbounded))
                .count() % 2 == 1
            {
                kept_paths.push(generator.borrow().paths[*path_index].path);
            }
        }
    }
}

fn create_events<K: Ord + HasX + Copy, P: BentleyOttmannPath<BentleyOttmannKey = K>>(
    paths: &[ClippingPath<P>],
) -> (Vec<ClassifyEvent>, HashMap<YCoordinate, Vec<PathIndex>>) {
    let mut raw_events = HashMap::with_capacity(2 * paths.len());
    let mut horizontal_paths = HashMap::new();
    for (index, path) in paths.iter().enumerate() {
        if path.as_ref().is_horizontal() {
            let y = YCoordinate(path.as_ref().points().0.y);
            horizontal_paths
                .entry(y)
                .or_insert_with(Vec::new)
                .push(index);
        } else {
            let (first_point, last_point) = path.as_ref().ordered_points();
            raw_events
                .entry(YCoordinate(first_point.y))
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .0
                .push(index);
            raw_events
                .entry(YCoordinate(last_point.y))
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .1
                .push(index);
        }
    }

    let mut events: Vec<_> = raw_events.into_iter().map(|(k, v)| (k, v.0, v.1)).collect();
    events.sort_by(|a, b| b.0.cmp(&a.0));
    (events, horizontal_paths)
}
