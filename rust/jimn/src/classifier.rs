//! inclusion tree builder
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::collections::Bound::*;

use {ElementaryPath, HoledPolygon, Pocket, Polygon, Segment};
use bentley_ottmann::{check_keys_validity, BentleyOttmannPath, HasX, KeyGenerator, PathIndex,
                      YCoordinate};
use dyntreap::Treap;
use tree::Tree;
use quadrant::Shape;
//use std::collections::Bound;
//use utils::debug::AsDebug;
//use tycat::colored_display;

/// Container is a pocket or a polygon.

/// Each container has an outer edge composed of paths.
pub trait Contains<P> {
    /// Return outer edge in form of path iterator
    fn edge<'a>(&'a self) -> Box<Iterator<Item = P> + 'a>;
}

impl Contains<Segment> for Polygon {
    fn edge<'a>(&'a self) -> Box<Iterator<Item = Segment> + 'a> {
        Box::new(self.segments())
    }
}

impl Contains<Segment> for HoledPolygon {
    fn edge<'a>(&'a self) -> Box<Iterator<Item = Segment> + 'a> {
        Box::new(self.polygon.segments())
    }
}

impl Contains<ElementaryPath> for Pocket {
    fn edge<'a>(&'a self) -> Box<Iterator<Item = ElementaryPath> + 'a> {
        Box::new(self.edge.iter().cloned())
    }
}

type ClassifyEvent = (YCoordinate, Vec<PathIndex>, Vec<PathIndex>);
type ContainerIndex = usize;

/// we need to remember which path belongs to which container
#[derive(Debug)]
struct OwnedPath<P> {
    path: P,
    owner: ContainerIndex,
}

impl<P> AsRef<P> for OwnedPath<P> {
    fn as_ref(&self) -> &P {
        &self.path
    }
}

/// The `Classifier` structure holds all data needed for building inclusion tree.
struct Classifier<
    's,
    'k: 's,
    'p: 'k,
    't: 'p,
    K: 'k + Ord + Eq + Copy + HasX,
    P: 'p + BentleyOttmannPath<BentleyOttmannKey = K>,
    C: Contains<P> + 't,
> {
    /// Final result
    inclusion_tree: &'t mut Tree<C>,

    /// We store currently crossed paths in a treap (again their positions in input vector).
    crossed_paths: Treap<'s, K, PathIndex>,

    /// We store the key generator for our own path comparison purposes.
    key_generator: Rc<RefCell<KeyGenerator<'k, K, P, OwnedPath<P>>>>,

    /// Store alive paths, by containers.
    /// Using this we can easily iterate on all paths from a given container.
    alive_paths: HashMap<ContainerIndex, HashSet<PathIndex>>,
    phantom: PhantomData<&'p P>,
}

impl<
    's,
    'k: 's,
    'p: 'k,
    't: 'p,
    K: 'k + Ord + Copy + Eq + HasX,
    P: 'p + BentleyOttmannPath<BentleyOttmannKey = K> + Shape,
    C: Contains<P> + Shape + Default,
> Classifier<'s, 'k, 'p, 't, K, P, C>
{
    /// Create all owned paths and events.
    fn new(
        tree: &'t mut Tree<C>,
        paths: &'k mut Vec<OwnedPath<P>>,
        containers: Vec<C>,
    ) -> (Vec<ClassifyEvent>, Classifier<'s, 'k, 'p, 't, K, P, C>) {
        // immediately add all containers as tree nodes
        // this way we can use their position in tree as their id
        // NOTE that it is important that their container_id is larger than the ids
        // of previous containers. in this way previous containers events will come first on
        // overlapping cases (because sort of events is stable).
        for container in containers {
            tree.add_node(container);
        }

        // we continue by adding all paths from existing leaves.
        *paths = tree.nodes
            .iter()
            .filter(|n| n.children.is_empty())
            .flat_map(|node| {
                let index = node.index;
                node.value
                    .edge()
                    .filter(|p| !p.is_horizontal())
                    .map(move |p| OwnedPath {
                        path: p,
                        owner: index,
                    })
            })
            .collect();

        let mut raw_events = HashMap::new();

        for (index, path) in paths.iter().enumerate() {
            let (first_point, last_point) = path.path.ordered_points();
            raw_events
                .entry(YCoordinate(first_point.y))
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .0
                .push(index);
            raw_events
                .entry(YCoordinate(last_point.y))
                .or_insert_with(|| (Vec::new(), Vec::new()))
                .1
                .push(index);
        }

        let mut events: Vec<_> = raw_events.into_iter().map(|(k, v)| (k, v.0, v.1)).collect();
        events.sort_by(|a, b| b.0.cmp(&a.0));
        let generator = KeyGenerator::new(paths);

        //TODO: could we avoid looping twice ?
        for (index, path) in paths.iter().enumerate() {
            let (first_point, last_point) = path.path.ordered_points();
            for point in &[&first_point, &last_point] {
                let y = YCoordinate(point.y);
                let mut key = path.path.compute_key(y);
                key.set_x(point.x);
                generator.borrow_mut().keys_cache.insert((index, y), key);
            }
        }

        // sort start events
        for event in &mut events {
            generator.borrow_mut().current_y = event.0;
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
                crossed_paths: Treap::new_with_key_generator(get_key),
                key_generator: generator,
                alive_paths: HashMap::new(),
                phantom: PhantomData,
            },
        )
    }

    /// Execute all events, building containers tree.
    fn run(&mut self, events: &[ClassifyEvent]) {
        for event in events {
            // remove ending paths
            self.end_paths(&event.2);
            self.key_generator.borrow_mut().current_y = event.0;
            self.start_paths(&event.1);
        }
    }

    /// End given paths.
    fn end_paths(&mut self, paths: &[PathIndex]) {
        for path in paths {
            self.crossed_paths
                .remove(&self.key_generator.borrow().compute_key(path));
            let owner = self.key_generator.borrow().paths[*path].owner;
            self.alive_paths.get_mut(&owner).unwrap().remove(path);
        }
    }

    /// Add given paths in treap, classify new containers.
    fn start_paths(&mut self, paths: &[PathIndex]) {
        // add everyone and classify new containers on the fly
        for path in paths {
            self.crossed_paths.insert(*path);
            let owner = self.key_generator.borrow().paths[*path].owner;
            self.alive_paths
                .entry(owner)
                .or_insert_with(HashSet::new)
                .insert(*path);
            if self.inclusion_tree.father(owner).is_none() {
                // not classified yet
                self.classify_container(owner, path);
            }
        }
        if cfg!(debug_assertions) {
            check_keys_validity(&self.crossed_paths, &self.key_generator);
        }
    }

    /// Add given container at right place in containers tree.
    fn classify_container(&mut self, owner: ContainerIndex, path_index: &PathIndex) {
        let father_id; // where to connect us ?
        let limit = self.key_generator.borrow().compute_key(path_index);
        let nearest_node = self.crossed_paths
            .ordered_nodes((Included(limit), Unbounded))
            .find(|n| n.value != *path_index);
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

    /// Is path with given key included in given container ?
    fn inclusion_test(&self, key: K, container: ContainerIndex) -> bool {
        let count = self.alive_paths[&container]
            .iter()
            .filter(|p| self.key_generator.borrow().compute_key(p) >= key)
            .count();
        (count % 2) == 1
    }
}

/// Complement given tree by classifying given containers into it.
/// New containers can only arrive below leaves or below new nodes.
pub fn complete_inclusion_tree<
    'k,
    'p: 'k,
    't: 'p,
    K: 'k + Ord + Eq + Copy + HasX,
    P: 'p + BentleyOttmannPath<BentleyOttmannKey = K> + Shape,
    C: Contains<P> + Shape + Default,
>(
    tree: &'t mut Tree<C>,
    containers: Vec<C>,
) {
    let mut paths = Vec::new();
    let (events, mut classifier) = Classifier::new(tree, &mut paths, containers);
    classifier.run(&events);
}
