//! Main treap structures.
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use std::marker::PhantomData;
use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;
use super::{Node, Counter, EmptyCounter, Counting, KeyComputer, UniqueKey};

/// sequential counter for tycat files
static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

/// classic Treap
pub type Treap<T, V, W> = RawTreap<T, EmptyCounter, V, W>;
/// Treap where subtrees sizes are counted. This allows to count number of larger nodes
/// in O(log(n))
pub type CountingTreap<T, V, W> = RawTreap<T, Counter, V, W>;

/// Treap BST structure.
/// This structure is specialized for sweeping line algorithms and contains
/// the current position (used in paths comparisons).
pub struct RawTreap<T, U, V, W>
    where T: Default + Eq,
          U: Counting,
          V: Ord,
          W: KeyComputer<T, V>
{
    root: Node<T, U>,
    key_generator: Rc<RefCell<W>>,
    ghost: PhantomData<V>,
}

impl<T: Default + Eq, U: Counting, V: Ord, W: KeyComputer<T, V>> RawTreap<T, U, V, W> {
    /// Creates a new Treap.
    pub fn new(key_generator: Rc<RefCell<W>>) -> RawTreap<T, U, V, W> {
        let tree = RawTreap {
            root: Node::new(Default::default()),
            key_generator: key_generator,
            ghost: PhantomData,
        };
        tree.root.borrow_mut().priority = 0;
        tree
    }

    /// Do we contain any non-root node ?
    pub fn empty(&self) -> bool {
        self.root.child(1).is_none()
    }

    /// Iterate on all non-root nodes (not in order).
    pub fn nodes(&self) -> DepthFirstIterator<T, U> {
        DepthFirstIterator {
            remaining_nodes: if let Some(start) = self.root.child(1) {
                vec![start]
            } else {
                Vec::new()
            },
        }
    }

    /// Fills the tree with given content.
    pub fn populate<X: IntoIterator<Item = T>>(&self, content: X) {
        for value in content {
            self.add(value);
        }
    }

    /// Returns Node with given value or None.
    /// # Example
    /// ```
    /// use jimn::tree::treap::{IdentityKeyComputer, Treap};
    /// use std::rc::Rc;
    /// use std::cell::RefCell;
    /// let tree = Treap::new(Rc::new(RefCell::new(IdentityKeyComputer())));
    /// tree.populate(1..10);
    /// let node5 = tree.find_node(5);
    /// assert!(node5.is_some());
    /// let node = node5.unwrap();
    /// assert_eq!(node.borrow().value, 5);
    /// ```
    pub fn find_node(&self, value: T) -> Option<Node<T, U>> {
        //let mut current_node = self.root.clone();
        let possible_start = self.root.child(1);
        if possible_start.is_some() {
            let mut current_node = possible_start.unwrap();
            let target_key = self.key_generator.borrow().compute_key(&value);
            while current_node.borrow().value != value {
                let current_key =
                    self.key_generator.borrow().compute_key(&current_node.borrow().value);
                let direction = (target_key > current_key) as usize;
                if let Some(next_node) = current_node.child(direction) {
                    current_node = next_node;
                } else {
                    return None;
                }
            }
            Some(current_node)
        } else {
            None
        }
    }

    /// Returns the place where to insert given new value.
    pub fn find_insertion_place(&self, key: &V) -> (Node<T, U>, usize) {
        let mut current_node = self.root.clone();
        let mut direction = 1; // because sentinel has min key
        while let Some(next_node) = current_node.child(direction) {
            current_node = next_node;
            let node_key = self.key_generator.borrow().compute_key(&current_node.borrow().value);
            direction = (*key > node_key) as usize;
        }
        (current_node, direction as usize)
    }

    /// Adds a node to treap with given value.
    pub fn add(&self, value: T) -> Node<T, U> {
        let key = self.key_generator.borrow().compute_key(&value);
        let (mut current_node, direction) = self.find_insertion_place(&key);
        current_node.add_child_with_value(direction, value)
    }
}

impl<T: Display + Default + Eq, U: Counting, V: Ord, W: KeyComputer<T, V>> RawTreap<T, U, V, W> {
    /// Tycat display on terminal.
    pub fn tycat(&self) {
        let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
        let dot_filename = format!("/tmp/test-{}.dot", file_number);
        let png_filename = format!("/tmp/test-{}.png", file_number);
        {
            let mut file = File::create(&dot_filename).expect("cannot create dot file");
            writeln!(file, "digraph g {{").expect("failed writing dot");
            self.root.write_dot(&mut file);
            writeln!(file, "}}").expect("failed writing dot");
        }
        Command::new("dot")
            .arg("-Tpng")
            .arg(&dot_filename)
            .arg("-o")
            .arg(&png_filename)
            .status()
            .expect("dot failed");
        Command::new("tycat").arg(&png_filename).status().expect("tycat failed");
    }
}


impl<T: Default + Eq, V: Ord, W: KeyComputer<T, V>> CountingTreap<T, V, W> {
    /// Return how many nodes are larger or equal to given key.
    pub fn number_of_larger_nodes(&self, key: &V) -> usize {
        let (mut node, _) = self.find_insertion_place(key);
        // key might be present a second time in our left subtree
        // check if it is there.
        let mut total = if let Some(ref child) = node.child(0) {
            let extreme_node = child.extreme_node(1);
            let extreme_key = self.key_generator.borrow().compute_key(&extreme_node.borrow().value);
            if extreme_key == *key { 1 } else { 0 }
        } else {
            0
        };
        while !node.is_root() {
            let node_key = self.key_generator.borrow().compute_key(&node.borrow().value);
            if node_key >= *key {
                total += 1;
                if let Some(ref child) = node.child(1) {
                    total += child.borrow().counter.0;
                }
            }
            node = node.father();
        }
        total
    }
}

impl<T: Default + Eq, U: Counting, V: UniqueKey, W: KeyComputer<T, V>> RawTreap<T, U, V, W> {
    /// Returns node with same real key (if any).
    pub fn find_same_node(&self, key: &V) -> Option<Node<T, U>> {
        let mut current_node = self.root.clone();
        let mut direction = 1; // because sentinel has min key
        while let Some(next_node) = current_node.child(direction) {
            current_node = next_node;
            let node_key = self.key_generator.borrow().compute_key(&current_node.borrow().value);
            if node_key.is_same_as(key) {
                return Some(current_node.clone());
            }
            direction = (*key > node_key) as usize;
        }
        None
    }
}

/// Depth first iterator on all Nodes.
pub struct DepthFirstIterator<T, U: Counting> {
    remaining_nodes: Vec<Node<T, U>>,
}

impl<T, U: Counting> Iterator for DepthFirstIterator<T, U> {
    type Item = Node<T, U>;
    fn next(&mut self) -> Option<Node<T, U>> {
        if let Some(next_node) = self.remaining_nodes.pop() {
            self.remaining_nodes.extend((0..2).into_iter().filter_map(|d| next_node.child(d)));
            Some(next_node)
        } else {
            None
        }
    }
}
