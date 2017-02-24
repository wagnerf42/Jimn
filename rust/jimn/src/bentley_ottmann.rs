//! Bentley Ottmann intersection algorithm.
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, BinaryHeap};
use ordered_float::NotNaN;
use point::Point;
use segment::Segment;
use tree::treap::{Treap, KeyComputer, Node};
use utils::ArrayMap;
use utils::coordinates_hash::PointsHash;

//for tycat
use quadrant::{Quadrant, Shape};
use tycat::display;


///We need someone able to compute comparison keys for our segments.
#[derive(Clone, Debug)]
struct KeyGenerator<'a, 'b, 'c> {
    /// Where we currently are.
    current_point: &'a RefCell<Point>,
    /// We need a reference to our segments in order to perform index <-> segment conversion.
    segments: &'b [Segment],
    /// Computing keys requires to know sweping lines intersections.
    x_coordinates: &'c RefCell<HashMap<(usize, NotNaN<f64>), NotNaN<f64>>>,
}

impl<'a, 'b, 'c> KeyComputer<usize, (NotNaN<f64>, NotNaN<f64>)> for KeyGenerator<'a, 'b, 'c> {
    fn compute_key(&self, segment: &usize) -> (NotNaN<f64>, NotNaN<f64>) {
        let current_x = self.current_point.borrow().x;
        let current_y = self.current_point.borrow().y;
        let s = &self.segments[*segment];
        let angle = s.sweeping_angle();
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
            let table = self.x_coordinates.borrow();
            let stored_x = table.get(&key);
            if let Some(&x) = stored_x {
                x
            } else {
                s.horizontal_line_intersection(current_y)
                    .expect("computing key for non intersecting segment")
            }
        };

        if current_x > x {
            // we are not yet arrived on intersection
            (x, -angle)
        } else {
            // we are past the intersection
            (x, angle)
        }
    }
}

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter<'a, 'b, 'c, 'd> {
    //TODO: could we replace the hashset by a vector ?
    /// Results: we associate to each segment (identified by it's position in input vector)
    /// a set of intersections.
    intersections: HashMap<usize, HashSet<Point>>,

    /// Where we currently are.
    current_point: &'a RefCell<Point>,

    /// Remaining events.
    events: BinaryHeap<Point>,

    /// X coordinates for comparison key computations.
    /// We store for each segment (identified by it's position in input vector) and y coordinate
    /// the corresponding x coordinate when intersecting segment with horizontal line passing
    /// through y.
    /// Storing this information allows to avoid a lot of rounding errors and it is crucial
    /// to the correctness of the algorithm.
    x_coordinates: &'c RefCell<HashMap<(usize, NotNaN<f64>), NotNaN<f64>>>,

    //TODO: switch to vectors
    //we would need to remember the pairs of segments already tested for intersections
    /// We store for each event point sets of segments ending and starting there.
    /// The use of set instead of vector allows us to not bother about intersections
    /// being detected twice.
    events_data: HashMap<Point, [HashSet<usize>; 2]>,

    /// We store currently crossed segments in a treap (again their positions in input vector).
    crossed_segments: Treap<usize, (NotNaN<f64>, NotNaN<f64>), KeyGenerator<'a, 'b, 'c>>,

    /// We store the key generator for our own segments comparison purposes.
    key_generator: KeyGenerator<'a, 'b, 'c>,

    /// Rounder for new points.
    rounder: &'d mut PointsHash,
}

impl<'a, 'b, 'c, 'd> Cutter<'a, 'b, 'c, 'd> {
    fn new(capacity: usize,
           current_point: &'a RefCell<Point>,
           x_coordinates: &'c RefCell<HashMap<(usize, NotNaN<f64>), NotNaN<f64>>>,
           segments: &'b [Segment],
           rounder: &'d mut PointsHash)
           -> Cutter<'a, 'b, 'c, 'd> {

        let generator = KeyGenerator {
            current_point: current_point,
            segments: segments,
            x_coordinates: x_coordinates,
        };

        let mut cutter = Cutter {
            intersections: HashMap::new(),
            current_point: current_point, // initial value does not matter
            events: BinaryHeap::new(),
            x_coordinates: x_coordinates,
            events_data: HashMap::with_capacity(capacity),
            crossed_segments: Treap::new(generator.clone()),
            key_generator: generator,
            rounder: rounder,
        };

        let mut x_coordinates = cutter.x_coordinates.borrow_mut();
        for (index, segment) in segments.iter().enumerate() {
            let (start, end) = segment.ordered_points();
            cutter.add_event(start, index, 0);
            cutter.add_event(end, index, 1);
            (*x_coordinates).insert((index, start.y), start.x);
            (*x_coordinates).insert((index, end.y), end.x);
        }
        cutter
    }

    /// Add event at given point starting or ending given segment.
    fn add_event(&mut self, event_point: Point, segment: usize, event_type: usize) {
        let events = &mut self.events;
        // if there is no event data it's a new event
        self.events_data.entry(event_point).or_insert_with(|| {
                events.push(event_point);
                [HashSet::new(), HashSet::new()]
            })
            [event_type]
            .insert(segment);
    }

    /// Try intersecting segments in two given nodes.
    fn try_intersecting(&mut self, node1: &Node<usize>, node2: &Node<usize>) {
        //TODO: use rounder for computing intersection
        let nodes = [node1, node2];
        let indices = nodes.map(|n| n.borrow().value);
        let segments = indices.map(|i| &self.key_generator.segments[*i]);
        let possible_intersection = segments[0].intersection_with(segments[1]);
        //TODO: use if let
        if possible_intersection.is_some() {
            let intersection = self.rounder.hash_point(&possible_intersection.unwrap());
            if intersection >= *self.key_generator.current_point.borrow() {
                return; // too late, we are already aware of it !
            }
            for (index, segment) in indices.iter().zip(segments.iter()) {
                if !segment.has_endpoint(&intersection) {
                    self.x_coordinates
                        .borrow_mut()
                        .insert((*index, intersection.y), intersection.x);
                    self.add_event(intersection, *index, 0);
                    self.add_event(intersection, *index, 1);
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
    fn end_segments(&mut self, segments: &mut Vec<usize>) {
        if segments.is_empty() {
            return;
        }
        segments.sort_by(|a, b| {
            self.key_generator.compute_key(a).cmp(&self.key_generator.compute_key(b))
        });
        let small_node = self.crossed_segments
            .find_node(*segments.first().unwrap())
            .expect("ending : no small node in crossed segments");
        let small_neighbour = small_node.nearest_node(0);
        if small_neighbour.is_some() {
            let big_node = self.crossed_segments.find_node(*segments.last().unwrap()).unwrap();
            let big_neighbour = big_node.nearest_node(1);
            if big_neighbour.is_some() {
                self.try_intersecting(&small_node, &big_node);
            }
        }
        for segment in segments {
            self.crossed_segments.find_node(*segment).unwrap().remove();
        }
    }

    /// Start a set of segments.
    /// Checks for possible intersections to add in the system.
    fn start_segments(&mut self, segments: &mut Vec<usize>) {
        if segments.is_empty() {
            return;
        }
        segments.sort_by(|a, b| {
            self.key_generator.compute_key(a).cmp(&self.key_generator.compute_key(b))
        });

        for segment in segments.iter() {
            self.crossed_segments.add(*segment);
        }

        let small_node = self.crossed_segments.find_node(*segments.first().unwrap()).unwrap();
        let small_neighbour = small_node.nearest_node(0);
        if small_neighbour.is_some() {
            self.try_intersecting(&small_node, &small_neighbour.unwrap());
        }
        let big_node = self.crossed_segments.find_node(*segments.last().unwrap()).unwrap();
        let big_neighbour = big_node.nearest_node(1);
        if big_neighbour.is_some() {
            self.try_intersecting(&big_node, &big_neighbour.unwrap());
        }
    }

    /// Main algorithm's loop.
    fn run(&mut self) {
        while !self.events.is_empty() {
            let event_point = self.events.pop().unwrap();
            let mut starting_segments: Vec<usize>;
            let mut ending_segments: Vec<usize>;
            {
                let changing_segments = &self.events_data[&event_point];
                starting_segments = changing_segments[0].iter().cloned().collect();
                ending_segments = changing_segments[1].iter().cloned().collect();
            }
            self.end_segments(&mut ending_segments);
            *self.current_point.borrow_mut() = event_point;
            self.start_segments(&mut starting_segments);
            self.events_data.remove(&event_point);
        }
    }
}

/// Computes all intersections amongst given segments
/// and return vector of obtained elementary segments.
pub fn bentley_ottmann(segments: &[Segment], rounder: &mut PointsHash) -> Vec<Segment> {
    // I need to declare current point outside of the main structure to avoid cyclic constructors
    // problems. (sigh)
    let current_point = RefCell::new(Point::new(0.0, 0.0));
    //guess the capacity of all our events related hash tables.
    //we need to be above truth to avoid collisions but not too much above.
    let capacity = 8 * segments.len();
    // again, to avoid lifetimes problems I need to declare this outside of main struct.
    let x_coordinates = RefCell::new(HashMap::with_capacity(capacity));

    let mut cutter = Cutter::new(capacity, &current_point, &x_coordinates, segments, rounder);
    cutter.run();

    let points: Vec<&Point> =
        cutter.intersections.values().flat_map(|points| points.iter()).collect();
    display!(segments, points);
    Vec::new()
}
