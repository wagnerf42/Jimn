//! Bentley Ottmann intersection algorithm.
//! TODO: document: no more that 2 overlapping segments at any place.
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, BinaryHeap};
use ordered_float::NotNaN;
use {Point, Segment};
use tree::treap::{Treap, KeyComputer};
use utils::ArrayMap;
use utils::coordinates_hash::PointsHash;

//some type aliases for more readability
type Coordinate = NotNaN<f64>;
type Angle = NotNaN<f64>;
/// A `SegmentIndex` allows identification of a segment in sweeping line algorihtms.
pub type SegmentIndex = usize;

/// A `Key` allows segments comparisons in sweeping line algorithms.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub struct Key(Coordinate, Angle);

///We need someone able to compute comparison keys for our segments.
#[derive(Debug)]
pub struct KeyGenerator<'a, T: 'a + AsRef<Segment>> {
    /// Where we currently are.
    pub current_point: Point,
    /// We need a reference to our paths in order to perform index <-> path conversion.
    pub paths: &'a [T],
    /// Computing keys requires to know sweeping lines intersections.
    pub x_coordinates: HashMap<(SegmentIndex, Coordinate), Coordinate>,
    /// Cache angles for each path
    angles_cache: Vec<NotNaN<f64>>,
}

impl<'a, T: AsRef<Segment>> KeyGenerator<'a, T> {
    /// Create a key generator from segments.
    pub fn new(paths: &'a [T]) -> Rc<RefCell<KeyGenerator<'a, T>>> {
        let angles_cache = paths
            .iter()
            .map(|p| p.as_ref().sweeping_angle())
            .collect();
        Rc::new(RefCell::new(KeyGenerator {
                                 //initial current point does not matter
                                 current_point: Default::default(),
                                 paths: paths,
                                 x_coordinates: HashMap::with_capacity(3 * paths.len()),
                                 angles_cache: angles_cache,
                             }))
    }
}

impl<'a, T: AsRef<Segment>> KeyComputer<SegmentIndex, Key> for KeyGenerator<'a, T> {
    fn compute_key(&self, segment: &SegmentIndex) -> Key {
        let (current_x, current_y) = self.current_point.coordinates();
        let s = self.paths[*segment].as_ref();
        let angle = self.angles_cache[*segment];
        let x = if s.is_horizontal() {
            current_x
        } else {
            // sweep goes from bottom to top and right to left
            //
            //     |    \    /   |       --- on this line xa > xb (no angle needed)
            //  l1 |     \  /    | l2
            //     |      \/     |         ____ now here, at start of l2 a < b and
            //            /\                    at start of l1 a > b
            //           /  \                   xa == xb so we use angles
            //          /    \       ---- on this line xa < xb (no angle needed)
            //         a      b
            //
            //         angle of a is 3pi/4 ; angle of b is pi/4
            let key = (*segment, current_y);
            let stored_x = self.x_coordinates.get(&key);
            if let Some(&x) = stored_x {
                x
            } else {
                s.horizontal_line_intersection(current_y)
                    .expect("computing key for non intersecting segment")
            }
        };

        if current_x > x {
            // we are not yet arrived on intersection
            Key(x, -angle)
        } else {
            // we are past the intersection
            Key(x, angle)
        }
    }
}

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter<'a, 'b, T: 'a + AsRef<Segment>> {
    //TODO: could we replace the hashset by a vector ?
    /// Results: we associate to each segment (identified by it's position in input vector)
    /// a set of intersections.
    intersections: HashMap<SegmentIndex, HashSet<Point>>,

    /// Remaining events.
    events: BinaryHeap<Point>,

    //TODO: switch to vectors
    //we would need to remember the pairs of segments already tested for intersections
    /// We store for each event point sets of segments ending and starting there.
    /// The use of set instead of vector allows us to not bother about intersections
    /// being detected twice.
    events_data: HashMap<Point, [HashSet<SegmentIndex>; 2]>,

    /// We store the key generator for our own segments comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'a, T>>>,

    /// Rounder for new points.
    rounder: &'b mut PointsHash,
}

impl<'a, 'b, T: 'a + AsRef<Segment>> Cutter<'a, 'b, T> {
    fn new(paths: &'a [T],
           rounder: &'b mut PointsHash)
           -> (Cutter<'a, 'b, T>, Treap<SegmentIndex, Key, KeyGenerator<'a, T>>) {

        //guess the capacity of all our events related hash tables.
        //we need to be above truth to avoid collisions but not too much above.
        let generator = KeyGenerator::new(paths);
        let crossed_segments = Treap::new(generator.clone());

        let mut cutter = Cutter {
            intersections: HashMap::new(),
            events: BinaryHeap::new(),
            events_data: HashMap::with_capacity(paths.len()),
            key_generator: generator,
            rounder: rounder,
        };

        for (index, path) in paths.iter().enumerate() {
            let (start, end) = path.as_ref().ordered_points();
            cutter.add_event(start, index, 0);
            cutter.add_event(end, index, 1);
            cutter
                .key_generator
                .borrow_mut()
                .x_coordinates
                .insert((index, start.y), start.x);
            cutter
                .key_generator
                .borrow_mut()
                .x_coordinates
                .insert((index, end.y), end.x);
        }
        (cutter, crossed_segments)
    }

    /// Add event at given point starting or ending given segment.
    fn add_event(&mut self, event_point: Point, path: SegmentIndex, event_type: usize) {
        let events = &mut self.events;
        // if there is no event data it's a new event
        self.events_data
            .entry(event_point)
            .or_insert_with(|| {
                                events.push(event_point);
                                [HashSet::new(), HashSet::new()]
                            })
            [event_type]
                .insert(path);
    }

    /// Try intersecting given segments.
    fn try_intersecting(&mut self, indices: [SegmentIndex; 2]) {
        let segments = indices.map(|i| &self.key_generator.borrow().paths[*i]);
        let current_point = self.key_generator.borrow().current_point;
        let possible_intersection = segments[0]
            .as_ref()
            .intersection_with(segments[1].as_ref());
        if let Some(raw_intersection) = possible_intersection {
            let intersection = self.rounder.hash_point(&raw_intersection);
            if intersection > current_point {
                return; // we already know about it
            }
            for (index, segment) in indices.iter().zip(segments.iter()) {
                if !segment.as_ref().has_endpoint(&intersection) {
                    self.key_generator
                        .borrow_mut()
                        .x_coordinates
                        .insert((*index, intersection.y), intersection.x);
                    if intersection < current_point {
                        // it is possible to be equal in case of overlapping segments
                        self.add_event(intersection, *index, 0);
                        self.add_event(intersection, *index, 1);
                    }
                    self.intersections
                        .entry(*index)
                        .or_insert_with(HashSet::new)
                        .insert(intersection);
                }
            }
        }
    }

    /// End a set of segments.
    /// Checks for possible intersections to add in the system.
    fn end_segments(&mut self,
                    segments: &mut Vec<SegmentIndex>,
                    crossed_segments: &mut Treap<SegmentIndex, Key, KeyGenerator<'a, T>>) {
        if segments.is_empty() {
            return;
        }
        let mut sorted_segments: Vec<_> = segments
            .iter()
            .map(|s| (self.key_generator.borrow().compute_key(s), s))
            .collect();

        sorted_segments.sort();
        for &(ref key, _) in &sorted_segments {
            crossed_segments.remove(key);
        }
        let small_key = &sorted_segments.first().unwrap().0;
        let big_key = &sorted_segments.last().unwrap().0;
        for small in crossed_segments.neighbouring_values(small_key, 0) {
            for big in crossed_segments.neighbouring_values(big_key, 1) {
                self.try_intersecting([small, big]);
            }
        }
    }

    /// Start a set of segments.
    /// Checks for possible intersections to add in the system.
    fn start_segments(&mut self,
                      segments: &mut Vec<SegmentIndex>,
                      crossed_segments: &mut Treap<SegmentIndex, Key, KeyGenerator<'a, T>>) {
        if segments.is_empty() {
            return;
        }

        let mut sorted_segments: Vec<_> = segments
            .iter()
            .map(|s| (self.key_generator.borrow().compute_key(s), s))
            .collect();

        sorted_segments.sort();

        for &(ref new_key, segment) in &sorted_segments {
            let same_key_node = crossed_segments.find_key(new_key);

            if let Some(overlap_node) = same_key_node {
                // handle overlaps
                let overlap_index = overlap_node.borrow().value;
                self.handle_overlapping_segments(*segment, overlap_index);
            }
            crossed_segments.add(*segment);
        }

        let small_key = &sorted_segments.first().unwrap().0;
        for &(_, added_small) in sorted_segments.iter().take_while(|t| t.0 == *small_key) {
            for existing_small in crossed_segments.neighbouring_values(small_key, 0) {
                self.try_intersecting([*added_small, existing_small]);
            }
        }
        let big_key = &sorted_segments.last().unwrap().0;
        for &(_, added_big) in sorted_segments
                .iter()
                .rev()
                .take_while(|t| t.0 == *big_key) {
            for existing_big in crossed_segments.neighbouring_values(big_key, 1) {
                self.try_intersecting([*added_big, existing_big]);
            }
        }
    }

    fn handle_overlapping_segments(&mut self, index1: SegmentIndex, index2: SegmentIndex) {
        let generator = self.key_generator.borrow();
        let s1 = &generator.paths[index1];
        let s2 = &generator.paths[index2];
        if let Some(points) = s1.as_ref().overlap_points(s2.as_ref()) {
            for point in &points {
                for &(segment, index) in &[(s1, index1), (s2, index2)] {
                    if !segment.as_ref().has_endpoint(point) {
                        self.intersections
                            .entry(index)
                            .or_insert_with(HashSet::new)
                            .insert(*point);
                    }
                }
            }
        }
    }

    /// Main algorithm's loop.
    fn run(&mut self, crossed_segments: &mut Treap<SegmentIndex, Key, KeyGenerator<'a, T>>) {
        while !self.events.is_empty() {
            let event_point = self.events.pop().unwrap();
            let mut starting_segments: Vec<SegmentIndex>;
            let mut ending_segments: Vec<SegmentIndex>;
            {
                let changing_segments = &self.events_data[&event_point];
                starting_segments = changing_segments[0].iter().cloned().collect();
                ending_segments = changing_segments[1].iter().cloned().collect();
            }
            self.end_segments(&mut ending_segments, crossed_segments);
            self.key_generator.borrow_mut().current_point = event_point;
            self.start_segments(&mut starting_segments, crossed_segments);
            self.events_data.remove(&event_point);
        }
    }
}

/// Computes all intersections amongst given segments
/// and return a hashmap associating to each segment's index the set of intersection points found.
pub fn bentley_ottmann<T: AsRef<Segment>>(segments: &[T],
                                          rounder: &mut PointsHash)
                                          -> HashMap<usize, HashSet<Point>> {

    let (mut cutter, mut crossed_segments) = Cutter::new(segments, rounder);
    cutter.run(&mut crossed_segments);
    cutter.intersections
}

/// A path is `Cuttable` if you can cut it into subpaths at given points.
pub trait Cuttable {
    /// Cut path at all given points.
    fn cut(&self, points: &HashSet<Point>) -> Vec<Self> where Self: Sized;
}

/// Cut all segments with intersection points obtained from `bentley_ottmann`.
pub fn cut_segments<T: Cuttable + Clone>(segments: &[T],
                                         cut_points: &HashMap<SegmentIndex, HashSet<Point>>)
                                         -> Vec<T> {
    segments
        .iter()
        .enumerate()
        .flat_map(|(i, segment)| {
                      let cuts = cut_points.get(&i);
                      if let Some(points) = cuts {
                          segment.cut(points)
                      } else {
                          vec![segment.clone()]
                      }
                  })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use segment::load_segments;

    fn prepare_tests(filename: &str) -> (PointsHash, Vec<Segment>) {
        let segments = load_segments(filename).expect("error loading segments file");
        let mut rounder = PointsHash::new(6);
        for segment in &segments {
            rounder.hash_point(&segment.start);
            rounder.hash_point(&segment.end);
        }
        (rounder, segments)
    }

    #[test]
    fn simple_overlap_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/overlap.bo");
        let results = bentley_ottmann(&segments, &mut rounder);
        let intersections_number: usize = results.values().map(|v| v.len()).sum();
        assert!(intersections_number == 2);
    }

    #[test]
    fn simple_case_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/simple_three.bo");
        let results = bentley_ottmann(&segments, &mut rounder);
        let intersections_number: usize = results.values().map(|v| v.len()).sum();
        assert!(intersections_number == 6);
    }

    #[test]
    fn simple_hexagonal_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/triangle_h_1.0.bo");
        let results = bentley_ottmann(&segments, &mut rounder);
        let intersections_number: usize = results.values().map(|v| v.len()).sum();
        assert!(intersections_number == 30);
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
