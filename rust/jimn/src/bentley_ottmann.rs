//! Bentley Ottmann intersection algorithm.
//! TODO: document: no more that 2 overlapping segments at any place.
use std::f64::consts::PI;
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
use utils::float::hash_float;
use std::hash::{Hash, Hasher};

//precision for float comparisons
const ULPS: i64 = 4;

//some type aliases for more readability
type Coordinate = f64;
type Angle = f64;
/// A `PathIndex` allows identification of a path in sweeping line algorithms.
pub type PathIndex = usize;

/// Y coordinates for sweeping line positions.
#[derive(Copy, Clone, Debug)]
pub struct YCoordinate(pub f64);

impl PartialEq for YCoordinate {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl Eq for YCoordinate {}
impl Ord for YCoordinate {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}
impl PartialOrd for YCoordinate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for YCoordinate {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_float(&self.0, state)
    }
}

impl Default for YCoordinate {
    fn default() -> Self {
        YCoordinate(0.0)
    }
}

/// We need to manually adjust x coordinates in keys.
pub trait HasX {
    /// Change x value in comparison key.
    fn set_x(&mut self, new_x: f64);
    /// Compute a new key with given x and minimal angles.
    fn min_key(x: f64) -> Self;
    /// Compute a new key with given x and maximal angles.
    fn max_key(x: f64) -> Self;
}

/// A `Key` allows segments comparisons in sweeping line algorithms.
#[derive(Copy, Clone)]
pub struct Key(Coordinate, Angle);

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0) && self.1.approx_eq_ulps(&other.1, ULPS)
    }
}
impl Eq for Key {}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .partial_cmp(&other.0)
            .unwrap()
            .then(self.1.approx_cmp(&other.1, ULPS))
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

impl HasX for Key {
    fn set_x(&mut self, new_x: f64) {
        self.0 = new_x
    }
    fn min_key(x: f64) -> Self {
        Key(x, 0.0)
    }
    fn max_key(x: f64) -> Self {
        Key(x, 2.0 * PI)
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
        self.0.approx_eq_ulps(&other.0, ULPS) && self.1.approx_eq_ulps(&other.1, ULPS)
            && self.2.approx_eq_ulps(&other.2, ULPS)
    }
}
impl Eq for ComplexKey {}

impl Ord for ComplexKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .approx_cmp(&other.0, ULPS)
            .then(self.1.approx_cmp(&other.1, ULPS))
            .then(self.2.approx_cmp(&other.2, ULPS))
    }
}

impl PartialOrd for ComplexKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl HasX for ComplexKey {
    fn set_x(&mut self, new_x: f64) {
        self.0 = new_x
    }
    fn min_key(x: f64) -> Self {
        ComplexKey(x, 0.0, 0.0)
    }
    fn max_key(x: f64) -> Self {
        ComplexKey(x, 2.0 * PI, 2.0 * PI)
    }
}

/// A `BentleyOttmannPath` is usable in sweeping line algorithms.
/// can in fact be segments or arcs.
pub trait BentleyOttmannPath: Shape {
    /// Paths need to be comparable and should provide comparison keys.
    type BentleyOttmannKey: Ord + Eq + HasX;
    /// Compute key for given path at given position.
    fn compute_key(&self, current_y: YCoordinate) -> Self::BentleyOttmannKey;
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

fn compute_segment_key(segment: &Segment, current_y: YCoordinate) -> Key {
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
    let x = segment.horizontal_line_intersection(current_y.0);
    Key(x, angle)
}

fn compute_arc_key(arc: &Arc, current_y: YCoordinate) -> ComplexKey {
    let intersection_point = arc.horizontal_line_intersection(current_y.0)
        .expect("computing key for non intersecting arc");
    let tangent = arc.tangent_angle(&intersection_point);
    let going_to = min(arc.start, arc.end);
    let final_angle = Segment::new(intersection_point, going_to).sweeping_angle();
    ComplexKey(intersection_point.x, tangent, final_angle)
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
    fn compute_key(&self, current_y: YCoordinate) -> Key {
        compute_segment_key(self, current_y)
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
    fn compute_key(&self, current_y: YCoordinate) -> ComplexKey {
        match *self {
            ElementaryPath::Arc(ref a) => compute_arc_key(a, current_y),
            ElementaryPath::Segment(ref s) => {
                let Key(coordinate, angle) = compute_segment_key(s, current_y);
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
pub struct KeyGenerator<
    'a,
    K: Ord + HasX,
    P: BentleyOttmannPath<BentleyOttmannKey = K>,
    T: 'a + AsRef<P>,
> {
    /// Where we currently are.
    pub current_y: YCoordinate,
    /// We need a reference to our paths in order to perform index <-> path conversion.
    pub paths: &'a [T],
    phantom1: PhantomData<K>,
    phantom2: PhantomData<P>,
    keys_cache: HashMap<(PathIndex, YCoordinate), K>,
}

impl<'a, K: Ord + HasX, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: AsRef<P>>
    KeyGenerator<'a, K, P, T> {
    /// Create a key generator from paths.
    pub fn new(paths: &'a [T]) -> Rc<RefCell<KeyGenerator<'a, K, P, T>>> {
        Rc::new(RefCell::new(KeyGenerator {
            //initial current point does not matter
            current_y: Default::default(),
            paths: paths,
            phantom1: PhantomData,
            phantom2: PhantomData,
            keys_cache: HashMap::with_capacity(2 * paths.len()),
        }))
    }
}

impl<'a, K: Ord + HasX + Copy, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: AsRef<P>>
    KeyGenerator<'a, K, P, T> {
    /// Return comparison key for given paths at current position.
    pub fn compute_key(&self, path_index: &PathIndex) -> K {
        self.keys_cache
            .get(&(*path_index, self.current_y))
            .map(|&k| k)
            .unwrap_or_else(|| {
                self.paths[*path_index].as_ref().compute_key(self.current_y)
            })
    }
}

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter<'a, 'b, K: Ord + HasX, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: 'a + AsRef<P>>
{
    //TODO: could we replace the hashset by a vector ?
    /// Results: we associate to each path (identified by it's position in input vector)
    /// a set of intersections.
    intersections: HashMap<PathIndex, HashSet<Point>>,

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
            rounder,
            horizontal_segments: HashMap::new(),
        };

        //TODO : for now, skip horizontal paths
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
                if !path.as_ref().has_endpoint(&intersection) {
                    if intersection.y < current_y.0 {
                        //TODO: checky check also
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
        for &(ref key, _) in &sorted_paths {
            crossed_paths.remove(key);
            for small in crossed_paths.neighbouring_values(*key, 0) {
                for big in crossed_paths.neighbouring_values(*key, 1) {
                    self.try_intersecting([*small, *big]);
                }
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
        //TODO: we can reduce the intersections number with a more clever algorithm
        for &(key, path) in &sorted_paths {
            for small_neighbour in crossed_paths.neighbouring_values(key, 0) {
                self.try_intersecting([*small_neighbour, *path]);
            }
            for big_neighbour in crossed_paths.neighbouring_values(key, 1) {
                self.try_intersecting([*big_neighbour, *path]);
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
            let event_y = self.events.pop().unwrap();
            let mut starting_paths: Vec<PathIndex>;
            let mut ending_paths: Vec<PathIndex>;
            {
                let changing_paths = &self.events_data[&event_y];
                starting_paths = changing_paths[0].iter().cloned().collect();
                ending_paths = changing_paths[1].iter().cloned().collect();
            }
            self.end_paths(&mut ending_paths, crossed_paths);
            self.key_generator.borrow_mut().current_y = event_y;
            //            let mut previous_key = None;
            //            let bounds: (Bound<K>, Bound<K>) = (Unbounded, Unbounded);
            //            for path in crossed_paths.ordered_values(bounds) {
            //                let key = self.key_generator.borrow().compute_key(path);
            //                if let Some(previous) = previous_key {
            //                    if key < previous {
            //                        println!("current y is {:?}", event_y);
            //                        println!("invalid key for {}", path);
            //                        self._debug_display(crossed_paths);
            //                        panic!("tree is invalid");
            //                    }
            //                }
            //                previous_key = Some(key);
            //            }
            self.start_paths(&mut starting_paths, crossed_paths);
            self.handle_horizontal_segments(crossed_paths);
            self.events_data.remove(&event_y);
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
                for path_index in crossed_paths.ordered_values(bounds) {
                    self.try_intersecting([segment_index, *path_index]);
                }
            }
        }
    }

    fn _debug_display(&mut self, crossed_paths: &Treap<K, PathIndex>) {
        let bounds: (Bound<K>, Bound<K>) = (Unbounded, Unbounded);
        crossed_paths.tycat();
        colored_display(
            crossed_paths
                .ordered_values(bounds)
                .map(|p| self.key_generator.borrow().paths[*p].as_ref()),
        ).expect("display failed");
        for p in crossed_paths.ordered_values(bounds) {
            println!(
                "{} is {:?} : {:?}",
                p,
                self.key_generator.borrow().paths[*p].as_debug(),
                self.key_generator.borrow().compute_key(p).as_debug()
            );
        }
    }
}

/// Computes all intersections amongst given paths
/// and return a hashmap associating to each path's index the set of intersection points found.
pub fn bentley_ottmann<
    K: Ord + Copy + HasX,
    P: BentleyOttmannPath<BentleyOttmannKey = K>,
    T: AsRef<P>,
>(
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
    fn cut<'a, I: 'a + IntoIterator<Item = &'a Point>>(&self, points: I) -> Vec<Self>
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
    fn simple_case_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/simple_three.bo");
        let results = bentley_ottmann(&segments, &mut rounder);
        let unique_points: HashSet<&Point> = results.values().flat_map(|v| v).collect();
        let intersections_number = unique_points.len();
        assert!(intersections_number == 3);
    }

    #[test]
    fn simple_hexagonal_works() {
        let (mut rounder, segments) = prepare_tests("tests_bentley_ottmann/triangle_h_1.0.bo");
        let results = bentley_ottmann(&segments, &mut rounder);
        let unique_points: HashSet<&Point> = results.values().flat_map(|v| v).collect();
        let intersections_number = unique_points.len();
        println!("intersections_number is {}", intersections_number);
        assert!(intersections_number == 15);
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
