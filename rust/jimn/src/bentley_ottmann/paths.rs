//! paths and keys structures for sweeping line algorithms

use std::f64::consts::PI;
use std::cmp::Ordering;
use std::cmp::{max, min};
use std::rc::Rc;
use std::iter::{empty, once};
use std::marker::PhantomData;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::cell::RefCell;
use utils::float::hash_float;

use float_cmp::{ApproxEqUlps, ApproxOrdUlps};
use fnv::FnvHashMap;

use {Arc, ElementaryPath, Point, Segment};
use quadrant::Shape;

//precision for float comparisons
const ULPS: i64 = 10;

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
    /// Get x value from key.
    fn get_x(&self) -> f64;
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
        Some(self.cmp(other))
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
    fn get_x(&self) -> f64 {
        self.0
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
        self.0.eq(&other.0) && self.1.approx_eq_ulps(&other.1, ULPS)
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
        Some(self.cmp(other))
    }
}

impl HasX for ComplexKey {
    fn set_x(&mut self, new_x: f64) {
        self.0 = new_x
    }
    fn get_x(&self) -> f64 {
        self.0
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
    /// Intersecting with a sweeping line gives exactly one point.
    fn sweeping_line_intersection(&self, y: f64) -> Point;
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
    /// Are we a segment ? If yes, on which line are we located ?
    fn path_key(&self) -> Option<Point>;
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
    let intersection_point = arc.horizontal_line_intersection(current_y.0);
    let tangent = (arc.tangent_angle(&intersection_point) + PI) % PI;
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

    fn path_key(&self) -> Option<Point> {
        Some(self.line_key())
    }

    fn sweeping_line_intersection(&self, y: f64) -> Point {
        Point::new(self.horizontal_line_intersection(y), y)
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

    fn sweeping_line_intersection(&self, y: f64) -> Point {
        match *self {
            ElementaryPath::Arc(ref a) => a.horizontal_line_intersection(y),
            ElementaryPath::Segment(ref s) => Point::new(s.horizontal_line_intersection(y), y),
        }
    }

    fn path_key(&self) -> Option<Point> {
        match *self {
            ElementaryPath::Arc(_) => None,
            ElementaryPath::Segment(ref s) => Some(s.line_key()),
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
    /// Store all keys here for fast access and rounding errors avoidance
    pub keys_cache: FnvHashMap<(PathIndex, YCoordinate), K>,
    //keys_cache: HashMap<(PathIndex, YCoordinate), K>,
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
            keys_cache: FnvHashMap::with_capacity_and_hasher(2 * paths.len(), Default::default()),
            //keys_cache: HashMap::with_capacity(2 * paths.len()),
        }))
    }
}

impl<'a, K: Ord + HasX + Copy, P: BentleyOttmannPath<BentleyOttmannKey = K>, T: AsRef<P>>
    KeyGenerator<'a, K, P, T> {
    /// Return comparison key for given paths at current position.
    pub fn compute_key(&self, path_index: &PathIndex) -> K {
        self.keys_cache
            .get(&(*path_index, self.current_y))
            .cloned()
            .unwrap_or_else(|| {
                self.paths[*path_index].as_ref().compute_key(self.current_y)
            })
    }

    /// Return the point for given path at given y.
    pub fn point_at(&self, path_index: &PathIndex, y: &YCoordinate) -> Point {
        Point::new(
            self.keys_cache
                .get(&(*path_index, *y))
                .expect("getting x for missing key")
                .get_x(),
            y.0,
        )
    }
}

/// A path is `Cuttable` if you can cut it into subpaths at given points.
pub trait Cuttable {
    /// Cut path at all given points.
    fn cut<'a, I: 'a + IntoIterator<Item = &'a Point>>(&self, points: I) -> Vec<Self>
    where
        Self: Sized;
    /// Create a new subpath at given points from given path.
    /// Does not change points order.
    /// Prerequisite: p1 > p2
    fn new_from(&self, p1: &Point, p2: &Point) -> Self;
}

impl Cuttable for Segment {
    /// Cut into subsegments at given points.
    /// pre-requisite: all given points are strictly inside us.
    /// they contain no duplicate
    ///
    /// # Example
    /// ```
    /// use jimn::{Point, Segment};
    /// use jimn::bentley_ottmann::Cuttable;
    /// let p1 = Point::new(0.0, 0.0);
    /// let p2 = Point::new(1.0, 1.0);
    /// let p3 = Point::new(2.0, 2.0);
    /// let p4 = Point::new(3.0, 3.0);
    /// let s = Segment::new(p1.clone(), p4.clone());
    /// let p = vec![p2.clone(), p3.clone()];
    /// let segments = s.cut(&p);
    /// println!("{:?}", segments);
    /// assert!(segments[0] == Segment::new(p1, p2.clone()));
    /// assert!(segments[1] == Segment::new(p2, p3.clone()));
    /// assert!(segments[2] == Segment::new(p3, p4));
    /// assert!(segments.len() == 3);
    /// ```
    fn cut<'a, I: 'a + IntoIterator<Item = &'a Point>>(&self, points: I) -> Vec<Segment> {
        let mut sorted_points: Vec<&Point> = points.into_iter().collect();
        if self.start < self.end {
            sorted_points.sort();
        } else {
            sorted_points.sort_by(|a, b| b.cmp(a));
        }

        let iterator = once(&self.start).chain(sorted_points.into_iter().chain(once(&self.end)));
        iterator
            .clone()
            .zip(iterator.skip(1))
            .map(|(p1, p2)| Segment::new(*p1, *p2))
            .collect()
    }

    fn new_from(&self, p1: &Point, p2: &Point) -> Self {
        if self.start > self.end {
            Segment::new(*p1, *p2)
        } else {
            Segment::new(*p2, *p1)
        }
    }
}

impl Cuttable for Arc {
    fn cut<'a, I: 'a + IntoIterator<Item = &'a Point>>(&self, points: I) -> Vec<Self> {
        let mut sorted_points: Vec<&Point> = points.into_iter().collect();
        if self.start < self.end {
            sorted_points.sort();
        } else {
            sorted_points.sort_by(|a, b| b.cmp(a));
        }

        let iterator = once(&self.start).chain(sorted_points.into_iter().chain(once(&self.end)));
        iterator
            .clone()
            .zip(iterator.skip(1))
            .map(|(p1, p2)| Arc::new(*p1, *p2, self.center, self.radius))
            .collect()
    }

    fn new_from(&self, p1: &Point, p2: &Point) -> Self {
        if self.start > self.end {
            Arc::new(*p1, *p2, self.center, self.radius)
        } else {
            Arc::new(*p2, *p1, self.center, self.radius)
        }
    }
}

impl Cuttable for ElementaryPath {
    fn cut<'a, I: 'a + IntoIterator<Item = &'a Point>>(&self, points: I) -> Vec<Self> {
        match *self {
            ElementaryPath::Arc(a) => a.cut(points).into_iter().map(ElementaryPath::Arc).collect(),
            ElementaryPath::Segment(s) => s.cut(points)
                .into_iter()
                .map(ElementaryPath::Segment)
                .collect(),
        }
    }

    fn new_from(&self, start: &Point, end: &Point) -> Self {
        match *self {
            ElementaryPath::Arc(ref a) => ElementaryPath::Arc(a.new_from(start, end)),
            ElementaryPath::Segment(ref s) => ElementaryPath::Segment(s.new_from(start, end)),
        }
    }
}
