//! Bentley Ottmann intersection algorithm.
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, BinaryHeap};
use ordered_float::NotNaN;
use point::Point;
use segment::Segment;
use tree::treap::{Treap, KeyComputer, Node};

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
        if s.is_horizontal() {
            panic!("TODO: handle key for horizontal segments")
        } else {
            let x = s.horizontal_line_intersection(current_y)
                .expect("computing key for non intersecting segment");
            if x > current_x {
                (x, angle)
            } else {
                (x, -angle)
            }
        }
    }
}

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter<'a, 'b, 'c> {
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
    /// We store for each event point sets of segments ending and starting there.
    /// The use of set instead of vector allows us to not bother about intersections
    /// being detected twice.
    events_data: HashMap<Point, [HashSet<usize>; 2]>,

    /// We store currently crossed segments in a treap (again their positions in input vector).
    crossed_segments: Treap<usize, (NotNaN<f64>, NotNaN<f64>), KeyGenerator<'a, 'b, 'c>>,

    /// We store the key generator for our own segments comparison purposes.
    key_generator: KeyGenerator<'a, 'b, 'c>,
}

impl<'a, 'b, 'c> Cutter<'a, 'b, 'c> {
    fn new(capacity: usize,
           current_point: &'a RefCell<Point>,
           x_coordinates: &'c RefCell<HashMap<(usize, NotNaN<f64>), NotNaN<f64>>>,
           segments: &'b [Segment])
           -> Cutter<'a, 'b, 'c> {

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
        let s1 = &self.key_generator.segments[node1.borrow().value];
        let s2 = &self.key_generator.segments[node2.borrow().value];
        let possible_intersection = s1.intersection_with(s2);
        if possible_intersection.is_some() {
            let intersection = possible_intersection.unwrap();
            if intersection >= *self.key_generator.current_point.borrow() {
                return;
            }
            if !s1.has_endpoint(&intersection) {
                panic!("TODO: add intersection events");
            }
            if !s2.has_endpoint(&intersection) {
                panic!("TODO: add intersection events");
            }
        }
    }

    /// End a set of segments.
    /// Checks for possible intersections to add in the system.
    fn end_segments(&mut self, ended_segments: &mut HashSet<usize>) {
        let mut segments: Vec<usize> = ended_segments.drain().collect();
        if segments.is_empty() {
            return;
        }
        segments.sort_by(|a, b| {
            self.key_generator.compute_key(a).cmp(&self.key_generator.compute_key(b))
        });
        let small_node = self.crossed_segments.find_node(*segments.first().unwrap()).unwrap();
        let small_neighbour = small_node.nearest_node(0);
        if small_neighbour.is_some() {
            let big_node = self.crossed_segments.find_node(*segments.last().unwrap()).unwrap();
            let big_neighbour = big_node.nearest_node(1);
            if big_neighbour.is_some() {
                self.try_intersecting(&small_node, &big_node);
            }
        }
        for segment in segments {
            self.crossed_segments.find_node(segment).unwrap().remove();
        }
    }

    /// Start a set of segments.
    /// Checks for possible intersections to add in the system.
    fn start_segments(&mut self, started_segments: &mut HashSet<usize>) {
        let mut segments: Vec<usize> = started_segments.drain().collect();
        if segments.is_empty() {
            return;
        }
        segments.sort_by(|a, b| {
            self.key_generator.compute_key(a).cmp(&self.key_generator.compute_key(b))
        });

        for segment in &segments {
            self.crossed_segments.add(*segment);
        }
        self.crossed_segments.tycat();

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
            let mut event_data = self.events_data.remove(&event_point).expect("no event data");
            self.end_segments(&mut event_data[1]);
            *self.current_point.borrow_mut() = event_point;
            self.start_segments(&mut event_data[0]);
        }
    }
}

/// Computes all intersections amongst given segments
/// and return vector of obtained elementary segments.
pub fn bentley_ottmann(segments: &[Segment]) -> Vec<Segment> {
    // I need to declare current point outside of the main structure to avoid cyclic constructors
    // problems. (sigh)
    let current_point = RefCell::new(Point::new(0.0, 0.0));
    //guess the capacity of all our events related hash tables.
    //we need to be above truth to avoid collisions but not too much above.
    let capacity = 8 * segments.len();
    // again, to avoid lifetimes problems I need to declare this outside of main struct.
    let x_coordinates = RefCell::new(HashMap::with_capacity(capacity));

    Cutter::new(capacity, &current_point, &x_coordinates, segments).run();
    panic!("TODO bentley ottmann");
}
