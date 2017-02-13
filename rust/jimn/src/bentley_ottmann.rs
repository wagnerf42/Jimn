//! Bentley Ottmann intersection algorithm.
use std::collections::{HashMap, HashSet, BTreeSet, BinaryHeap};
use ordered_float::NotNaN;
use point::Point;
use segment::Segment;

/// The `Cutter` structure holds all data needed for bentley ottmann's execution.
struct Cutter {
    /// Results: we associate to each segment (identified by it's position in input vector)
    /// a set of intersections.
    intersections: HashMap<usize, HashSet<Point>>,

    /// Current position during algorithm's execution.
    current_position: Point,

    /// Remaining events.
    events: BinaryHeap<Point>,

    /// X coordinates for comparison key computations.
    /// We store for each segment (identified by it's position in input vector) and y coordinate
    /// the corresponding x coordinate when intersecting segment with horizontal line passing
    /// through y.
    /// Storing this information allows to avoid a lot of rounding errors and it is crucial
    /// to the correctness of the algorithm.
    x_coordinates: HashMap<(usize, NotNaN<f64>), NotNaN<f64>>,

    //TODO: switch to vectors
    /// We store for each event point sets of segments ending and starting there.
    /// The use of set instead of vector allows us to not bother about intersections
    /// being detected twice.
    events_data: HashMap<Point, [HashSet<usize>; 2]>,
}

impl Cutter {
    fn new(segments: &[Segment]) -> Cutter {

        //guess the capacity of all our events related hash tables.
        //we need to be above truth to avoid collisions but not too much above.
        let capacity = 8 * segments.len();

        let mut cutter = Cutter {
            intersections: HashMap::new(),
            current_position: Point::new(0.0, 0.0), //does not matter
            events: BinaryHeap::new(),
            x_coordinates: HashMap::with_capacity(capacity),
            events_data: HashMap::with_capacity(capacity),
        };

        for (index, segment) in segments.iter().enumerate() {
            let (start, end) = segment.ordered_points();
            cutter.add_event(start, index, 0);
            cutter.add_event(end, index, 1);
            cutter.x_coordinates.insert((index, start.y), start.x);
            cutter.x_coordinates.insert((index, end.y), end.x);
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

    fn run(&self) -> HashMap<usize, HashSet<Point>> {
        for point in &self.events {
            println!("event point is {:?}", point);
        }
        //while !self.events.is_empty() {
        //}
        panic!("TODO");
    }
}

/// Computes all intersections amongst given segments
/// and return vector of obtained elementary segments.
pub fn bentley_ottmann(segments: &[Segment]) -> Vec<Segment> {
    Cutter::new(segments).run();
    panic!("TODO");
}
