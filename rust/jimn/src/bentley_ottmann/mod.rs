//! Bentley Ottmann intersection algorithm.
//! TODO: document: no more that 2 overlapping segments at any place.
use std::cell::RefCell;
use std::collections::{BinaryHeap, HashMap, HashSet};
use dyntreap::Treap;
use utils::ArrayMap;
use utils::coordinates_hash::PointsHash;
use std::rc::Rc;
use std::iter::repeat;

use {Point, Segment};
use quadrant::Quadrant;
use utils::debug::AsDebug;
use std::collections::Bound::*;
use std::collections::Bound;

mod paths;
pub use self::paths::{BentleyOttmannPath, Cuttable, HasX, Key, KeyGenerator, PathIndex,
                      YCoordinate};

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter<'a, 'b, K: Ord + HasX, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: 'a + AsRef<P>>
{
    /// Small paths obtained after cutting
    results: Vec<T>,

    /// On each path, which point was previously visited ?
    previous_points: Vec<Point>,

    /// Remaining events.
    events: BinaryHeap<YCoordinate>,

    /// We store for each event point sets of paths ending and starting there.
    /// The use of set instead of vector allows us to not bother about intersections
    /// being detected twice.
    events_data: HashMap<YCoordinate, [HashSet<PathIndex>; 2]>,

    /// We store the key generator for our own paths comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'a, K, P, T>>>,

    /// Horizontal segments now get a special treatment.
    horizontal_segments: HashMap<YCoordinate, Vec<PathIndex>>,

    /// Round new points immediately
    rounder: &'b mut PointsHash,
}

impl<
    'a,
    'b,
    K: 'a + Ord + Copy + HasX,
    P: 'a + BentleyOttmannPath<BentleyOttmannKey = K>,
    T: 'a + AsRef<P> + Cuttable,
> Cutter<'a, 'b, K, P, T>
{
    fn new(
        paths: &'a [T],
        rounder: &'b mut PointsHash,
    ) -> (Cutter<'a, 'b, K, P, T>, Treap<'a, K, PathIndex>) {
        //guess the capacity of all our events related hash tables.
        //we need to be above truth to avoid collisions but not too much above.
        let generator = KeyGenerator::new(paths);
        let closure_generator = Rc::clone(&generator);
        let get_key = move |index: &PathIndex| closure_generator.borrow().compute_key(index);
        let crossed_paths = Treap::new_with_key_generator(get_key);

        module_debug!({
            println!("starting new bentley ottmann");
            display!(unicolor!(paths.iter().map(|p| p.as_ref())));
        });

        let mut cutter = Cutter {
            results: Vec::with_capacity(2 * paths.len()),
            previous_points: repeat(Default::default()).take(paths.len()).collect(),
            events: BinaryHeap::new(),
            events_data: HashMap::with_capacity(paths.len()),
            key_generator: generator,
            rounder,
            horizontal_segments: HashMap::new(),
        };

        for (index, path) in paths.iter().enumerate() {
            if path.as_ref().is_horizontal() {
                let event_y = YCoordinate(path.as_ref().points().0.y);
                cutter
                    .horizontal_segments
                    .entry(event_y)
                    .or_insert_with(Vec::new)
                    .push(index);
                let events = &mut cutter.events;
                cutter.events_data.entry(event_y).or_insert_with(|| {
                    events.push(event_y);
                    [HashSet::new(), HashSet::new()]
                });
            } else {
                let (start, end) = path.as_ref().ordered_points();
                cutter.add_event(start, index, 0);
                cutter.add_event(end, index, 1);
            }
        }
        (cutter, crossed_paths)
    }

    /// Add event at given point starting or ending given paths.
    fn add_event(&mut self, event_point: Point, path: PathIndex, event_type: usize) {
        let events = &mut self.events;
        let event_y = YCoordinate(event_point.y);
        // if there is no event data it's a new event
        self.events_data.entry(event_y).or_insert_with(|| {
            events.push(event_y);
            [HashSet::new(), HashSet::new()]
        })[event_type]
            .insert(path);
        let mut key = self.key_generator.borrow().paths[path]
            .as_ref()
            .compute_key(event_y);

        key.set_x(event_point.x);

        self.key_generator
            .borrow_mut()
            .keys_cache
            .insert((path, event_y), key);
    }

    /// Try intersecting given paths.
    fn try_intersecting(&mut self, indices: [PathIndex; 2]) {
        let paths = indices.map(|i| &self.key_generator.borrow().paths[*i]);
        let current_y = self.key_generator.borrow().current_y;
        for intersection in paths[0].as_ref().intersections_with(paths[1].as_ref()) {
            let intersection = self.rounder.hash_point(&intersection);
            if intersection.y > current_y.0 {
                return; // we already know about it
            }
            for (index, path) in indices.iter().zip(paths.iter()) {
                if !path.as_ref().has_endpoint(&intersection) && intersection.y < current_y.0 {
                    //TODO: checky check also
                    // it is possible to be equal in case of overlapping segments
                    self.add_event(intersection, *index, 0);
                    self.add_event(intersection, *index, 1);
                }
            }
        }
    }

    /// End a set of paths.
    /// Checks for possible intersections to add in the system.
    fn end_paths(
        &mut self,
        paths: &mut Vec<PathIndex>,
        crossed_paths: &mut Treap<K, PathIndex>,
        next_y: &YCoordinate,
    ) {
        if paths.is_empty() {
            return;
        }

        let mut sorted_paths: Vec<_> = paths
            .iter()
            .map(|s| (self.key_generator.borrow().compute_key(s), s))
            .collect();

        //sorting is good for performances :-)
        sorted_paths.sort();
        for &(ref key, path) in &sorted_paths {
            {
                let end_point = self.key_generator.borrow().point_at(path, next_y);
                let start_point = &self.previous_points[*path];
                self.results.push(
                    self.key_generator.borrow().paths[*path].new_from(start_point, &end_point),
                );
            }
            crossed_paths.remove(key);
            if let Some(small) = crossed_paths.neighbouring_values(*key, 0).next() {
                if let Some(big) = crossed_paths.neighbouring_values(*key, 1).next() {
                    self.try_intersecting([*small, *big]);
                }
            }
        }
    }

    /// Start a set of paths.
    /// Checks for possible intersections to add in the system.
    fn start_paths(&mut self, paths: &[PathIndex], crossed_paths: &mut Treap<K, PathIndex>) {
        if paths.is_empty() {
            return;
        }

        let current_y = self.key_generator.borrow().current_y;
        for path in paths {
            let key = self.key_generator.borrow().compute_key(path);
            self.previous_points[*path] = self.key_generator.borrow().point_at(path, &current_y);
            crossed_paths.insert(*path);
            if let Some(small_neighbour) = crossed_paths.neighbouring_values(key, 0).next() {
                self.try_intersecting([*small_neighbour, *path]);
            }
            if let Some(big_neighbour) = crossed_paths.neighbouring_values(key, 1).next() {
                self.try_intersecting([*big_neighbour, *path]);
            }
        }
    }

    /// Main algorithm's loop.
    fn run(&mut self, crossed_paths: &mut Treap<K, PathIndex>) {
        while !self.events.is_empty() {
            let event_y = self.events.pop().unwrap();
            let starting_paths: Vec<PathIndex>;
            let mut ending_paths: Vec<PathIndex>;
            {
                let changing_paths = &self.events_data[&event_y];
                starting_paths = changing_paths[0].iter().cloned().collect();
                ending_paths = changing_paths[1].iter().cloned().collect();
            }
            self.end_paths(&mut ending_paths, crossed_paths, &event_y);
            self.key_generator.borrow_mut().current_y = event_y;
            if cfg!(debug_assertions) {
                check_keys_validity(crossed_paths, &self.key_generator);
            }
            self.start_paths(&starting_paths, crossed_paths);
            self.handle_horizontal_segments(crossed_paths);
            self.events_data.remove(&event_y);
            module_debug!({
                println!("bentley ottmann: after reaching new y");
                debug_display(crossed_paths, &self.key_generator);
            });
        }
    }

    /// Find and store all intersections for horizontal segments at current y.
    fn handle_horizontal_segments(&mut self, crossed_paths: &Treap<K, PathIndex>) {
        let y = self.key_generator.borrow().current_y;
        if let Some(segments) = self.horizontal_segments.remove(&y) {
            for segment_index in segments {
                let (max_point, min_point) = self.key_generator.borrow().paths[segment_index]
                    .as_ref()
                    .ordered_points();
                let bounds: (Bound<K>, Bound<K>) = (
                    Included(K::min_key(min_point.x)),
                    Included(K::max_key(max_point.x)),
                );
                let mut previous_point = min_point;
                for path_index in crossed_paths.ordered_values(bounds) {
                    let path_intersection = self.key_generator.borrow().paths[*path_index]
                        .as_ref()
                        .sweeping_line_intersection(y.0);
                    let path_intersection = self.rounder.hash_point(&path_intersection);
                    if previous_point != path_intersection {
                        self.results.push(
                            self.key_generator.borrow().paths[segment_index]
                                .new_from(&path_intersection, &previous_point),
                        );
                        previous_point = path_intersection;
                    }
                    let path_previous_point = self.previous_points[*path_index];
                    if path_previous_point.y != y.0 {
                        self.results.push(
                            self.key_generator.borrow().paths[*path_index]
                                .new_from(&path_previous_point, &path_intersection),
                        );
                        self.previous_points[*path_index] = path_intersection;
                    }
                }
                if previous_point != max_point {
                    self.results.push(
                        self.key_generator.borrow().paths[segment_index]
                            .new_from(&max_point, &previous_point),
                    );
                }
            }
        }
    }
}

/// Computes all intersections amongst given paths
/// and return a hashmap associating to each path's index the set of intersection points found.
pub fn bentley_ottmann<
    K: Ord + Copy + HasX,
    P: BentleyOttmannPath<BentleyOttmannKey = K>,
    T: AsRef<P> + Cuttable,
>(
    paths: &[T],
    rounder: &mut PointsHash,
) -> Vec<T> {
    let (mut cutter, mut crossed_paths) = Cutter::new(paths, rounder);
    cutter.run(&mut crossed_paths);
    cutter.results
}

// Debugging tools

/// Check one by one the all crossed paths keys are in valid order
pub fn check_keys_validity<
    K: Ord + HasX + Copy,
    P: BentleyOttmannPath<BentleyOttmannKey = K>,
    T: AsRef<P>,
>(
    crossed_paths: &Treap<K, PathIndex>,
    key_generator: &Rc<RefCell<KeyGenerator<K, P, T>>>,
) {
    let mut previous_key = None;
    let bounds: (Bound<K>, Bound<K>) = (Unbounded, Unbounded);
    for path in crossed_paths.ordered_values(bounds) {
        let key = key_generator.borrow().compute_key(path);
        if let Some(previous) = previous_key {
            if key < previous {
                println!("current y is {:?}", key_generator.borrow().current_y);
                println!("invalid key for {}", path);
                debug_display(crossed_paths, key_generator);
                panic!("tree is invalid");
            }
        }
        previous_key = Some(key);
    }
}

/// Display the whole tree of paths with their corresponding keys.
pub fn debug_display<
    K: Ord + HasX + Copy,
    P: BentleyOttmannPath<BentleyOttmannKey = K>,
    T: AsRef<P>,
>(
    crossed_paths: &Treap<K, PathIndex>,
    key_generator: &Rc<RefCell<KeyGenerator<K, P, T>>>,
) {
    let bounds: (Bound<K>, Bound<K>) = (Unbounded, Unbounded);
    crossed_paths.tycat();
    let mut quadrant = Quadrant::new(2);
    for sub_quadrant in crossed_paths
        .ordered_values(bounds)
        .map(|p| key_generator.borrow().paths[*p].as_ref().get_quadrant())
    {
        quadrant.update(&sub_quadrant);
    }
    let (x1, x2) = quadrant.limits(0);
    let y = key_generator.borrow().current_y.0;
    let current_line = Segment::new(Point::new(x1, y), Point::new(x2, y));
    display!(
        unicolor!(key_generator.borrow().paths.iter().map(|p| p.as_ref())),
        current_line,
        multicolor!(
            crossed_paths
                .ordered_values(bounds)
                .map(|p| key_generator.borrow().paths[*p].as_ref())
        )
    );
    for p in crossed_paths.ordered_values(bounds) {
        println!(
            "{} is {:?} : {:?}",
            p,
            key_generator.borrow().paths[*p].as_debug(),
            key_generator.borrow().compute_key(p).as_debug()
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use Segment;
    use segment::load_segments;
    use overlap::remove_overlaps;

    fn prepare_tests(filename: &str) -> (PointsHash, Vec<Segment>) {
        let segments = load_segments(filename).expect("error loading segments file");
        let mut rounder = PointsHash::new(6);
        for segment in &segments {
            rounder.hash_point(&segment.start);
            rounder.hash_point(&segment.end);
        }
        let no_overlap_segments = remove_overlaps(&segments);
        (rounder, no_overlap_segments)
    }

    #[test]
    fn simple_case_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/simple_three.bo");
        assert_eq!(bentley_ottmann(&segments, &mut rounder).len(), 9);
    }

    #[test]
    fn simple_hexagonal_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/triangle_h_1.0.bo");
        assert_eq!(bentley_ottmann(&segments, &mut rounder).len(), 141);
    }

    #[bench]
    fn bench_fast(b: &mut Bencher) {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/triangle_h_0.5.bo");
        b.iter(|| bentley_ottmann(&segments, &mut rounder));
    }

    #[bench]
    fn bench_slow(b: &mut Bencher) {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/carnifex_h_0.5.bo");
        b.iter(|| bentley_ottmann(&segments, &mut rounder));
    }
}
