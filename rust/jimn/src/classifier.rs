//! inclusion tree builder
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use quadrant::Shape;
use bentley_ottmann::{SegmentIndex, Key, KeyGenerator};
use point::Point;
use segment::Segment;
use polygon::Polygon;
use tree::Tree;
use tree::treap::{Treap, Node, KeyComputer, EmptyCounter};

/// We are enclosed in a polygon.
pub trait HasEdge {
    /// Return outer polygon;
    fn edge(&self) -> &Polygon;
}

type ClassifyEvent = (Point, Vec<SegmentIndex>, Vec<SegmentIndex>);
type PolygonIndex = usize;

/// we need to remember which segment belongs to which polygon
struct OwnedSegment {
    segment: Segment,
    owner: PolygonIndex,
}

impl AsRef<Segment> for OwnedSegment {
    fn as_ref(&self) -> &Segment {
        &self.segment
    }
}


/// The `Classifier` structure holds all data needed for building inclusion tree.
struct Classifier<'a, T: HasEdge> {
    /// Final result
    inclusion_tree: Tree<T>,

    /// We store currently crossed segments in a treap (again their positions in input vector).
    crossed_segments: Treap<SegmentIndex, Key, KeyGenerator<'a, OwnedSegment>>,

    /// We store the key generator for our own segments comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'a, OwnedSegment>>>,

    /// Store alive segments, by polygons.
    /// Using this we can easily iterate on all segments from a given polygon.
    alive_segments: HashMap<PolygonIndex, HashSet<SegmentIndex>>,
}

impl<'a, T: HasEdge + Shape + Default> Classifier<'a, T> {
    /// Create all segments and events.
    fn new(segments: &'a mut Vec<OwnedSegment>,
           polygons: Vec<T>)
           -> (Vec<ClassifyEvent>, Classifier<'a, T>) {

        let mut inclusion_tree = Tree::new();
        // immediately add all polygons as tree nodes
        // this way we can use their position in tree as their id
        for polygon in polygons {
            let polygon_index = inclusion_tree.next_node_index();
            segments.extend(polygon.edge()
                                .segments()
                                .filter(|s| !s.is_horizontal())
                                .map(|s| {
                                         OwnedSegment {
                                             segment: s,
                                             owner: polygon_index,
                                         }
                                     }));
            inclusion_tree.add_node(polygon);
        }

        let mut raw_events = HashMap::with_capacity(2 * segments.len());
        for (index, segment) in segments.iter().enumerate() {
            let (first_point, last_point) = segment.segment.ordered_points();
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

        let generator = KeyGenerator::new(segments);
        (events,
         Classifier {
             inclusion_tree: inclusion_tree,
             crossed_segments: Treap::new(generator.clone()),
             key_generator: generator,
             alive_segments: HashMap::new(),
         })
    }

    /// Execute all events, building polygons tree.
    fn run(&mut self, events: &[ClassifyEvent]) {
        for event in events {
            // remove ending segments
            self.end_segments(&event.2);
            self.key_generator.borrow_mut().current_point = event.0;
            self.start_segments(&event.1);
        }
    }

    /// End given segments.
    fn end_segments(&mut self, segments: &[SegmentIndex]) {
        for segment in segments {
            self.crossed_segments
                .find_node(*segment)
                .unwrap()
                .remove();
            let owner = self.key_generator.borrow().segments[*segment].owner;
            self.alive_segments
                .get_mut(&owner)
                .unwrap()
                .remove(segment);
        }
    }


    /// Add given segments in treap, classify new polygons.
    fn start_segments(&mut self, segments: &[SegmentIndex]) {
        // add everyone
        let mut nodes = Vec::with_capacity(segments.len());
        for segment in segments {
            nodes.push(self.crossed_segments.add(*segment));
            let owner = self.key_generator.borrow().segments[*segment].owner;
            self.alive_segments
                .entry(owner)
                .or_insert_with(HashSet::new)
                .insert(*segment);
        }

        // now, classify new polygons
        for node in &nodes {
            let segment_index = node.borrow().value;
            let owner = self.key_generator.borrow().segments[segment_index].owner;
            if self.inclusion_tree.father(owner).is_none() {
                // not classified yet
                self.classify_polygon(owner, segment_index, node);
            }
        }
    }

    /// Add given polygon at right place in polygons tree.
    fn classify_polygon(&mut self,
                        owner: PolygonIndex,
                        segment_index: SegmentIndex,
                        node: &Node<SegmentIndex, EmptyCounter>) {
        let father_id; // where to connect us ?
        if let Some(larger_neighbour) = node.nearest_node(1) {
            let neighbour_owner =
                self.key_generator.borrow().segments[larger_neighbour.borrow().value].owner;
            // we are either a brother of neighbour or its child
            let key = self.key_generator.borrow().compute_key(&segment_index);
            if self.inclusion_test(key, neighbour_owner) {
                // we are his child
                father_id = neighbour_owner;
            } else {
                // we are his brother
                father_id = self.inclusion_tree.father(neighbour_owner).unwrap();
            }
        } else {
            // we are son of root
            father_id = self.inclusion_tree.root();
        }
        self.inclusion_tree.set_child(father_id, owner)
    }


    /// Is segment with given key included in given polygon ?
    fn inclusion_test(&self, key: Key, polygon: PolygonIndex) -> bool {
        let count = self.alive_segments[&polygon]
            .iter()
            .filter(|s| self.key_generator.borrow().compute_key(s) > key)
            .count();
        (count % 2) == 1
    }
}

/// Return a tree saying which polygon is included into which one.
/// TODO: document what happens in case of overlap or partial overlap at wrong place
pub fn build_inclusion_tree<T: HasEdge + Shape + Default>(polygons: Vec<T>) -> Tree<T> {
    let mut segments = Vec::new();
    let (events, mut classifier) = Classifier::new(&mut segments, polygons);
    classifier.run(&events);
    classifier.inclusion_tree
}
