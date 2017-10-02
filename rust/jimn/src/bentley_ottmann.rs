//! Bentley Ottmann intersection algorithm.
//! TODO: document: no more that 2 overlapping segments at any place.
use std::cmp::Ordering;
use std::cmp::{max, min};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::iter::{empty, once};
use std::marker::PhantomData;
use {Arc, ElementaryPath, Point, Segment};
use dyntreap::Treap;
use utils::ArrayMap;
use utils::coordinates_hash::PointsHash;
use quadrant::Shape;
use float_cmp::{ApproxEqUlps, ApproxOrdUlps};
use std::fmt;

use utils::debug::AsDebug;
use tycat::colored_display;
use std::collections::Bound::*;
use std::collections::Bound;


//ROADMAP
// 4) angles caching to avoid ever recomputing
// 5) simplify logic by having one event for each path start/end instead of each point ?

//some type aliases for more readability
type Coordinate = f64;
type Angle = f64;
/// A `PathIndex` allows identification of a path in sweeping line algorithms.
pub type PathIndex = usize;

/// A `Key` allows segments comparisons in sweeping line algorithms.
#[derive(Copy, Clone)]
pub struct Key(Coordinate, Angle);

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.0.approx_eq_ulps(&other.0, 2) && self.1.approx_eq_ulps(&other.1, 2)
    }
}
impl Eq for Key {}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> Ordering {
        let first_part = self.0.approx_cmp(&other.0, 2);
        let second_part = self.1.approx_cmp(&other.1, 2);
        let result = self.0
            .approx_cmp(&other.0, 2)
            .then(self.1.approx_cmp(&other.1, 2));
        println!(
            "comparing {:?} and {:?} : {:?} ({:?},{:?})",
            self,
            other,
            result,
            first_part,
            second_part,
        );
        result
    }
}
impl PartialOrd for Key {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Key x: {:.20} a: {:.20}", self.0, self.1)
    }
}

/// `ComplexKey` allows for arcs and segments comparisons.
/// segment angles are twice the same (line support)
/// arc angles are tangent and segment angle between start and end.
/// this way we can compare arcs tangent to segments even on tangent points.
#[derive(Debug, Copy, Clone)]
pub struct ComplexKey(Coordinate, Angle, Angle);

impl PartialEq for ComplexKey {
    fn eq(&self, other: &Self) -> bool {
        self.0.approx_eq_ulps(&other.0, 2) && self.1.approx_eq_ulps(&other.1, 2)
            && self.2.approx_eq_ulps(&other.2, 2)
    }
}
impl Eq for ComplexKey {}

impl Ord for ComplexKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .approx_cmp(&other.0, 2)
            .then(self.1.approx_cmp(&other.1, 2))
            .then(self.2.approx_cmp(&other.2, 2))
    }
}

impl PartialOrd for ComplexKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

/// A `BentleyOttmannPath` is usable in sweeping line algorithms.
/// can in fact be segments or arcs.
pub trait BentleyOttmannPath: Shape {
    /// Paths need to be comparable and should provide comparison keys.
    type BentleyOttmannKey: Ord + Eq;
    /// Compute key for given path at given position.
    /// The stored_x is a cached part of the key to avoid rounding errors.
    fn compute_key(&self, current_point: &Point) -> Self::BentleyOttmannKey;
    /// Paths need to compute intersections between each others.
    fn intersections_with<'a>(&'a self, other: &'a Self) -> Box<Iterator<Item = Point> + 'a>;
    /// Paths have endpoints.
    fn points(&self) -> (Point, Point);
    /// Returns start and end point for sweeping line algorithm.
    /// We go for ymax to ymin and for equal ys from xmax to xmin.
    fn ordered_points(&self) -> (Point, Point) {
        let (start, end) = self.points();
        if start > end {
            (start, end)
        } else {
            (end, start)
        }
    }
    /// Do we have given point as endpoint ?
    fn has_endpoint(&self, potential_endpoint: &Point) -> bool {
        let points = self.points();
        points.0 == *potential_endpoint || points.1 == *potential_endpoint
    }
    /// If we overlap with given path return inside points.
    /// Pre-condition: we are aligned segments.
    /// Might return one duplicated point
    fn overlap_points(&self, other: &Self) -> Option<[Point; 2]>;
    /// Is the path a horizontal segment ?
    fn is_horizontal(&self) -> bool;
}

fn compute_segment_key(segment: &Segment, current_point: &Point) -> Key {
    let (current_x, current_y) = current_point.coordinates();
    let angle = segment.sweeping_angle();
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
    let x = if segment.is_horizontal() {
        current_x
    } else {
        segment
            .horizontal_line_intersection(current_y)
            .expect("computing key for non intersecting segment")
    };

    if current_x > x {
        // we are not yet arrived on intersection
        Key(x, -angle)
    } else {
        // we are past the intersection
        Key(x, angle)
    }
}

fn compute_arc_key(arc: &Arc, current_point: &Point) -> ComplexKey {
    let (current_x, current_y) = current_point.coordinates();
    let intersection_point = arc.horizontal_line_intersection(current_y)
        .expect("computing key for non intersecting arc");
    let tangent = arc.tangent_angle(&intersection_point);
    if current_x > intersection_point.x {
        // we are not yet at intersection
        let coming_from = max(arc.start, arc.end);
        let final_angle = Segment::new(intersection_point, coming_from).sweeping_angle();
        ComplexKey(intersection_point.x, -tangent, -final_angle)
    } else {
        // we are past intersection
        let going_to = min(arc.start, arc.end);
        let final_angle = Segment::new(intersection_point, going_to).sweeping_angle();
        ComplexKey(intersection_point.x, tangent, final_angle)
    }
}

fn segments_intersections(s1: &Segment, s2: &Segment) -> Box<Iterator<Item = Point>> {
    if let Some(point) = s1.intersection_with(s2) {
        Box::new(once(point))
    } else {
        Box::new(empty())
    }
}

fn segments_overlap_points(segment1: &Segment, segment2: &Segment) -> Option<[Point; 2]> {
    //     p2     p1
    // *---+------+--*
    let (max1, min1) = segment1.ordered_points();
    let (max2, min2) = segment2.ordered_points();
    let p1 = min(max1, max2);
    let p2 = max(min1, min2);
    if p1 >= p2 {
        Some([p1, p2])
    } else {
        None
    }
}


impl BentleyOttmannPath for Segment {
    type BentleyOttmannKey = Key;
    fn compute_key(&self, current_point: &Point) -> Key {
        compute_segment_key(self, current_point)
    }

    fn intersections_with(&self, other: &Self) -> Box<Iterator<Item = Point>> {
        segments_intersections(self, other)
    }
    fn points(&self) -> (Point, Point) {
        (self.start, self.end)
    }
    fn overlap_points(&self, other: &Segment) -> Option<[Point; 2]> {
        segments_overlap_points(self, other)
    }
    fn is_horizontal(&self) -> bool {
        self.is_horizontal()
    }
}

impl BentleyOttmannPath for ElementaryPath {
    type BentleyOttmannKey = ComplexKey;
    fn compute_key(&self, current_point: &Point) -> ComplexKey {
        match *self {
            ElementaryPath::Arc(ref a) => compute_arc_key(a, current_point),
            ElementaryPath::Segment(ref s) => {
                let Key(coordinate, angle) = compute_segment_key(s, current_point);
                ComplexKey(coordinate, angle, angle)
            }
        }
    }

    fn intersections_with<'a>(&'a self, other: &'a Self) -> Box<Iterator<Item = Point> + 'a> {
        match *self {
            ElementaryPath::Arc(ref self_arc) => match *other {
                ElementaryPath::Arc(ref other_arc) => {
                    Box::new(self_arc.intersections_with_arc(other_arc))
                }
                ElementaryPath::Segment(ref other_segment) => {
                    Box::new(self_arc.intersections_with_segment(other_segment))
                }
            },
            ElementaryPath::Segment(ref self_segment) => match *other {
                ElementaryPath::Arc(ref other_arc) => {
                    Box::new(other_arc.intersections_with_segment(self_segment))
                }
                ElementaryPath::Segment(ref other_segment) => {
                    segments_intersections(self_segment, other_segment)
                }
            },
        }
    }

    fn points(&self) -> (Point, Point) {
        match *self {
            ElementaryPath::Arc(ref a) => (a.start, a.end),
            ElementaryPath::Segment(ref s) => (s.start, s.end),
        }
    }

    fn overlap_points(&self, other: &ElementaryPath) -> Option<[Point; 2]> {
        match *self {
            ElementaryPath::Arc(_) => panic!("overlapping arcs"),
            ElementaryPath::Segment(ref s) => match *other {
                ElementaryPath::Arc(_) => panic!("overlapping arcs"),
                ElementaryPath::Segment(ref s2) => segments_overlap_points(s, s2),
            },
        }
    }
    fn is_horizontal(&self) -> bool {
        match *self {
            ElementaryPath::Arc(_) => false,
            ElementaryPath::Segment(ref s) => s.is_horizontal(),
        }
    }
}

///We need someone able to compute comparison keys for our paths.
#[derive(Debug)]
pub struct KeyGenerator<'a, K: Ord, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: 'a + AsRef<P>>
{
    /// Where we currently are.
    pub current_point: Point,
    /// We need a reference to our paths in order to perform index <-> path conversion.
    pub paths: &'a [T],
    phantom1: PhantomData<K>,
    phantom2: PhantomData<P>,
}

impl<'a, K: Ord, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: AsRef<P>>
    KeyGenerator<'a, K, P, T> {
    /// Create a key generator from paths.
    pub fn new(paths: &'a [T]) -> Rc<RefCell<KeyGenerator<'a, K, P, T>>> {
        Rc::new(RefCell::new(KeyGenerator {
            //initial current point does not matter
            current_point: Default::default(),
            paths: paths,
            phantom1: PhantomData,
            phantom2: PhantomData,
        }))
    }
}

impl<'a, K: Ord, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: AsRef<P>>
    KeyGenerator<'a, K, P, T> {
    /// Return comparison key for given paths at current position.
    pub fn compute_key(&self, path_index: &PathIndex) -> K {
        let path = self.paths[*path_index].as_ref();
        path.compute_key(&self.current_point)
    }
}

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter<'a, 'b, K: Ord, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: 'a + AsRef<P>> {
    //TODO: could we replace the hashset by a vector ?
    /// Results: we associate to each path (identified by it's position in input vector)
    /// a set of intersections.
    intersections: HashMap<PathIndex, HashSet<Point>>,

    /// Remaining events.
    events: BinaryHeap<Point>,

    //TODO: switch to vectors
    //we would need to remember the pairs of paths already tested for intersections
    /// We store for each event point sets of paths ending and starting there.
    /// The use of set instead of vector allows us to not bother about intersections
    /// being detected twice.
    events_data: HashMap<Point, [HashSet<PathIndex>; 2]>,

    /// We store the key generator for our own paths comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'a, K, P, T>>>,

    /// Rounder for new points.
    rounder: &'b mut PointsHash,
}


impl<
    'a,
    'b,
    K: 'a + Ord + Copy,
    P: 'a + BentleyOttmannPath<BentleyOttmannKey = K>,
    T: 'a + AsRef<P>,
> Cutter<'a, 'b, K, P, T> {
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
        }
        (cutter, crossed_paths)
    }

    /// Add event at given point starting or ending given paths.
    fn add_event(&mut self, event_point: Point, path: PathIndex, event_type: usize) {
        let events = &mut self.events;
        // if there is no event data it's a new event
        self.events_data.entry(event_point).or_insert_with(|| {
            events.push(event_point);
            [HashSet::new(), HashSet::new()]
        })[event_type]
            .insert(path);
    }

    /// Try intersecting given paths.
    fn try_intersecting(&mut self, indices: [PathIndex; 2]) {
        let paths = indices.map(|i| &self.key_generator.borrow().paths[*i]);
        let current_point = self.key_generator.borrow().current_point;
        for intersection in paths[0].as_ref().intersections_with(paths[1].as_ref()) {
            let intersection = self.rounder.hash_point(&intersection);
            if intersection > current_point {
                return; // we already know about it
            }
            for (index, path) in indices.iter().zip(paths.iter()) {
                if !path.as_ref().has_endpoint(&intersection) {
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

    /// End a set of paths.
    /// Checks for possible intersections to add in the system.
    fn end_paths(&mut self, paths: &mut Vec<PathIndex>, crossed_paths: &mut Treap<K, PathIndex>) {
        if paths.is_empty() {
            return;
        }
        let mut sorted_paths: Vec<_> = paths
            .iter()
            .map(|s| (self.key_generator.borrow().compute_key(s), s))
            .collect();

        sorted_paths.sort();
        for &(ref key, path) in &sorted_paths {
            println!(
                "removing {:?} with key {:?}",
                self.key_generator.borrow().paths[*path].as_debug(),
                key.as_debug()
            );
        }
        for &(ref key, _) in &sorted_paths {
            println!("removing key {:?}", key.as_debug());
            let bounds: (Bound<K>, Bound<K>) = (Unbounded, Unbounded);
            for value in crossed_paths.ordered_values(bounds) {
                println!("{:?}", self.key_generator.borrow().paths[*value].as_debug());
            }
            crossed_paths.remove(key);
        }
        let small_key = &sorted_paths.first().unwrap().0;
        let big_key = &sorted_paths.last().unwrap().0;
        for small in crossed_paths.neighbouring_values(*small_key, 0) {
            for big in crossed_paths.neighbouring_values(*big_key, 1) {
                self.try_intersecting([*small, *big]);
            }
        }
    }

    /// Start a set of paths.
    /// Checks for possible intersections to add in the system.
    fn start_paths(&mut self, paths: &mut Vec<PathIndex>, crossed_paths: &mut Treap<K, PathIndex>) {
        if paths.is_empty() {
            return;
        }

        let mut sorted_paths: Vec<_> = paths
            .iter()
            .map(|s| (self.key_generator.borrow().compute_key(s), s))
            .collect();

        sorted_paths.sort();
        for &(ref key, path) in &sorted_paths {
            println!(
                "adding {:?} with key {:?}",
                self.key_generator.borrow().paths[*path].as_debug(),
                key.as_debug()
            );
        }

        for &(ref new_key, path) in &sorted_paths {
            {
                let same_key_node = crossed_paths.get(new_key);

                if let Some(overlap_index) = same_key_node {
                    // handle overlaps
                    self.handle_overlapping_paths(*path, *overlap_index);
                }
            }
            crossed_paths.insert(*path);
        }

        let small_key = &sorted_paths.first().unwrap().0;
        for &(_, added_small) in sorted_paths.iter().take_while(|t| t.0 == *small_key) {
            for existing_small in crossed_paths.neighbouring_values(*small_key, 0) {
                self.try_intersecting([*added_small, *existing_small]);
            }
        }
        let big_key = &sorted_paths.last().unwrap().0;
        for &(_, added_big) in sorted_paths.iter().rev().take_while(|t| t.0 == *big_key) {
            for existing_big in crossed_paths.neighbouring_values(*big_key, 1) {
                self.try_intersecting([*added_big, *existing_big]);
            }
        }
    }

    fn handle_overlapping_paths(&mut self, index1: PathIndex, index2: PathIndex) {
        let generator = self.key_generator.borrow();
        let p1 = &generator.paths[index1];
        let p2 = &generator.paths[index2];
        if let Some(points) = p1.as_ref().overlap_points(p2.as_ref()) {
            for point in &points {
                for &(segment, index) in &[(p1, index1), (p2, index2)] {
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
    fn run(&mut self, crossed_paths: &mut Treap<K, PathIndex>) {
        while !self.events.is_empty() {
            let event_point = self.events.pop().unwrap();
            let mut starting_paths: Vec<PathIndex>;
            let mut ending_paths: Vec<PathIndex>;
            {
                let range: (Bound<K>, Bound<K>) = (Unbounded, Unbounded);
                colored_display(
                    crossed_paths
                        .ordered_values(range)
                        .map(|i| self.key_generator.borrow().paths[*i].as_ref()),
                ).expect("bad display");
                let changing_paths = &self.events_data[&event_point];
                starting_paths = changing_paths[0].iter().cloned().collect();
                ending_paths = changing_paths[1].iter().cloned().collect();
            }
            self.end_paths(&mut ending_paths, crossed_paths);
            println!("we are now at {}", event_point);
            self.key_generator.borrow_mut().current_point = event_point;
            self.start_paths(&mut starting_paths, crossed_paths);
            self.events_data.remove(&event_point);
        }
    }
}

/// Computes all intersections amongst given paths
/// and return a hashmap associating to each path's index the set of intersection points found.
pub fn bentley_ottmann<K: Ord + Copy, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: AsRef<P>>(
    paths: &[T],
    rounder: &mut PointsHash,
) -> HashMap<usize, HashSet<Point>> {
    let (mut cutter, mut crossed_paths) = Cutter::new(paths, rounder);
    cutter.run(&mut crossed_paths);
    cutter.intersections
}

/// A path is `Cuttable` if you can cut it into subpaths at given points.
pub trait Cuttable {
    /// Cut path at all given points.
    fn cut(&self, points: &HashSet<Point>) -> Vec<Self>
    where
        Self: Sized;
}

/// Cut all paths with intersection points obtained from `bentley_ottmann`.
pub fn cut_paths<T: Cuttable + Clone>(
    paths: &[T],
    cut_points: &HashMap<PathIndex, HashSet<Point>>,
) -> Vec<T> {
    paths
        .iter()
        .enumerate()
        .flat_map(|(i, path)| {
            let cuts = cut_points.get(&i);
            if let Some(points) = cuts {
                path.cut(points)
            } else {
                vec![path.clone()]
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
