//! inclusion tree builder
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::collections::Bound::*;

use quadrant::Shape;
use bentley_ottmann::{BentleyOttmannPath, Key, KeyGenerator, PathIndex};
use point::Point;
use segment::Segment;
use polygon::Polygon;
use dyntreap::Treap;
use tree::Tree;

/// We are enclosed in a polygon.
pub trait HasEdge {
    /// Return outer polygon;
    fn edge(&self) -> &Polygon;
}


type ClassifyEvent = (Point, Vec<PathIndex>, Vec<PathIndex>);
type PolygonIndex = usize;

/// we need to remember which segment belongs to which polygon
#[derive(Debug)]
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
struct Classifier<'a, 'b, T: HasEdge + 'b> {
    /// Final result
    inclusion_tree: &'b mut Tree<T>,

    /// We store currently crossed segments in a treap (again their positions in input vector).
    crossed_segments: Treap<'a, Key, PathIndex>,

    /// We store the key generator for our own segments comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'a, Key, Segment, OwnedSegment>>>,

    /// Store alive segments, by polygons.
    /// Using this we can easily iterate on all segments from a given polygon.
    alive_segments: HashMap<PolygonIndex, HashSet<PathIndex>>,
}

impl<'a, 'b, T: HasEdge + Shape + Default> Classifier<'a, 'b, T> {
    /// Create all segments and events.
    /// TODO: rewrite in a cleaner way
    fn new(
        tree: &'b mut Tree<T>,
        segments: &'a mut Vec<OwnedSegment>,
        polygons: Vec<T>,
    ) -> (Vec<ClassifyEvent>, Classifier<'a, 'b, T>) {
        // immediately add all polygons as tree nodes
        // this way we can use their position in tree as their id
        // NOTE that it is important that their polygon_id is larger than the ids
        // of previous polygons. in this way previous polygons events will come first on
        // overlapping cases.
        for polygon in polygons {
            tree.add_node(polygon);
        }

        // we continue by adding all segments from existing leaves.
        *segments = tree.nodes
            .iter()
            .filter(|n| n.children.is_empty())
            .flat_map(|node| {
                let index = node.index;
                node.value
                    .edge()
                    .segments()
                    .filter(|s| !s.is_horizontal())
                    .map(move |s| {
                        OwnedSegment {
                            segment: s,
                            owner: index,
                        }
                    })
            })
            .collect();


        let mut raw_events = HashMap::with_capacity(2 * segments.len());
        for (index, segment) in segments.iter().enumerate() {
            let (first_point, last_point) = segment.segment.ordered_points();
            raw_events
                .entry(first_point)
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .0
                .push(index);
            raw_events
                .entry(last_point)
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .1
                .push(index);
        }

        let mut events: Vec<_> = raw_events.into_iter().map(|(k, v)| (k, v.0, v.1)).collect();
        events.sort_by(|a, b| b.0.cmp(&a.0));
        let generator = KeyGenerator::new(segments);
        // sort start events
        for event in &mut events {
            generator.borrow_mut().current_point = event.0;
            //TODO: triple check this one
            event.1.sort_by(|a, b| {
                generator
                    .borrow()
                    .compute_key(b)
                    .cmp(&generator.borrow().compute_key(a))
            });
        }

        let closure_generator = Rc::clone(&generator);
        let get_key = move |index: &PathIndex| closure_generator.borrow().compute_key(index);
        (
            events,
            Classifier {
                inclusion_tree: tree,
                crossed_segments: Treap::new_with_key_generator(get_key),
                key_generator: generator,
                alive_segments: HashMap::new(),
            },
        )
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
    fn end_segments(&mut self, segments: &[PathIndex]) {
        for segment in segments {
            self.crossed_segments
                .remove(&self.key_generator.borrow().compute_key(segment));
            let owner = self.key_generator.borrow().paths[*segment].owner;
            self.alive_segments.get_mut(&owner).unwrap().remove(segment);
        }
    }


    /// Add given segments in treap, classify new polygons.
    fn start_segments(&mut self, segments: &[PathIndex]) {
        // add everyone and classify new polygons on the fly
        for segment in segments {
            self.crossed_segments.insert(*segment);
            let owner = self.key_generator.borrow().paths[*segment].owner;
            self.alive_segments
                .entry(owner)
                .or_insert_with(HashSet::new)
                .insert(*segment);
            if self.inclusion_tree.father(owner).is_none() {
                // not classified yet
                self.classify_polygon(owner, segment);
            }
        }
    }

    /// Add given polygon at right place in polygons tree.
    fn classify_polygon(&mut self, owner: PolygonIndex, segment_index: &PathIndex) {
        let father_id; // where to connect us ?
        let limit = self.key_generator.borrow().compute_key(segment_index);
        let nearest_node = self.crossed_segments
            .ordered_nodes((Included(limit), Unbounded))
            .filter(|n| n.value != *segment_index)
            .next();
        if let Some(larger_neighbour) = nearest_node {
            let neighbour_owner = self.key_generator.borrow().paths[larger_neighbour.value].owner;
            // we are either a brother of neighbour or its child
            if self.inclusion_test(limit, neighbour_owner) {
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
        self.inclusion_tree.set_child(father_id, owner);
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

/// Complement given tree by classifying given objects into it.
/// New objects can only arrive below leaves or below new nodes.
pub fn complete_inclusion_tree<T: HasEdge + Shape + Default>(tree: &mut Tree<T>, polygons: Vec<T>) {
    let mut segments = Vec::new();
    let (events, mut classifier) = Classifier::new(tree, &mut segments, polygons);
    classifier.run(&events);
}
