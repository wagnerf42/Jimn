//! inclusion tree builder
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use bentley_ottmann::{SegmentIndex, Key, KeyGenerator};
use point::Point;
use segment::Segment;
use polygon::Polygon;
use tree::{Tree, NodeIndex};
use tree::treap::{Treap, Node, KeyComputer};

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
struct Classifier<'a> {
    /// Final result
    inclusion_tree: Tree<Polygon>,

    /// We store currently crossed segments in a treap (again their positions in input vector).
    crossed_segments: Treap<SegmentIndex, Key, KeyGenerator<'a, OwnedSegment>>,

    /// We store the key generator for our own segments comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'a, OwnedSegment>>>,

    /// Store alive segments, by polygons.
    /// Using this we can easily iterate on all segments from a given polygon.
    alive_segments: HashMap<PolygonIndex, HashSet<SegmentIndex>>,

    /// Store polygons temporarily before insertion in tree.
    polygons: HashMap<PolygonIndex, Polygon>,

    /// quick access to polygons in tree (by polygonindex)
    polygon_nodes: HashMap<PolygonIndex, NodeIndex>,
}

impl<'a> Classifier<'a> {
    /// Create all segments and events.
    fn new(segments: &'a mut Vec<OwnedSegment>,
           polygons: Vec<Polygon>)
           -> (Vec<ClassifyEvent>, Classifier<'a>) {

        let mut stored_polygons: HashMap<PolygonIndex, Polygon> = HashMap::new();
        for (owner, polygon) in polygons.into_iter().enumerate() {
            for segment in polygon.points
                .iter()
                .zip(polygon.points.iter().cycle().skip(1))
                .map(|(&p1, &p2)| Segment::new(p1, p2)) {
                if !segment.is_horizontal() {
                    segments.push(OwnedSegment {
                        segment: segment,
                        owner: owner,
                    })
                }
            }
            stored_polygons.insert(owner, polygon);
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

        let mut events: Vec<_> = raw_events.into_iter()
            .map(|(k, v)| (k, v.0, v.1))
            .collect();
        events.sort_by(|a, b| b.0.cmp(&a.0));

        let generator = KeyGenerator::new(segments);
        (events,
         Classifier {
             inclusion_tree: Tree::new(), // no one for root
             crossed_segments: Treap::new(generator.clone()),
             key_generator: generator,
             alive_segments: HashMap::new(),
             polygons: stored_polygons,
             polygon_nodes: HashMap::new(),
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
            self.crossed_segments.find_node(*segment).unwrap().remove();
            let owner = self.key_generator.borrow().segments[*segment].owner;
            self.alive_segments.get_mut(&owner).unwrap().remove(segment);
        }
    }


    /// Add given segments in treap, classify new polygons.
    fn start_segments(&mut self, segments: &[SegmentIndex]) {
        // add everyone
        let mut nodes = Vec::with_capacity(segments.len());
        for segment in segments {
            nodes.push(self.crossed_segments.add(*segment));
            let owner = self.key_generator.borrow().segments[*segment].owner;
            self.alive_segments.entry(owner).or_insert_with(HashSet::new).insert(*segment);
        }

        // now, classify new polygons
        for node in &nodes {
            let segment_index = node.borrow().value;
            let owner = self.key_generator.borrow().segments[segment_index].owner;
            if self.polygons.contains_key(&owner) {
                self.classify_polygon(owner, segment_index, node);
            }
        }
    }

    /// Add given polygon at right place in polygons tree.
    fn classify_polygon(&mut self,
                        owner: PolygonIndex,
                        segment_index: SegmentIndex,
                        node: &Node<SegmentIndex>) {
        let polygon = self.polygons.remove(&owner).unwrap();
        let father_id; // where to add it
        if let Some(larger_neighbour) = node.nearest_node(1) {
            let neighbour_owner =
                self.key_generator.borrow().segments[larger_neighbour.borrow().value].owner;
            // we are either a brother of neighbour or its child
            let key = self.key_generator.borrow().compute_key(&segment_index);
            let neighbour_id = self.polygon_nodes[&neighbour_owner];
            if self.inclusion_test(key, neighbour_owner) {
                // we are his child
                father_id = neighbour_id;
            } else {
                // we are his brother
                father_id = self.inclusion_tree.father(neighbour_id);
            }
        } else {
            // we are son of root
            father_id = self.inclusion_tree.root();
        }
        let new_node_id = self.inclusion_tree.add_child(polygon, father_id);
        self.polygon_nodes.insert(owner, new_node_id);
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
pub fn build_inclusion_tree(polygons: Vec<Polygon>) -> Tree<Polygon> {
    let mut segments = Vec::new();
    let (events, mut classifier) = Classifier::new(&mut segments, polygons);
    classifier.run(&events);
    classifier.inclusion_tree
}
