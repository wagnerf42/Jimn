//! Provides a `Treap` structure for sweeping line algorithms.
use std::cell::RefCell;
use std::rc::Rc;
use std::ops::Deref;
use std::fs::File;
use std::process::Command;
use std::io::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::fmt::Display;
use std::marker::PhantomData;
use std::cmp::Ord;
use std;
use rand;

use utils::Identifiable;

/// sequential counter for tycat files
static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;


//TODO: we have leaks. look at weak references.

/// Objets inside a treap must be comparable by someone.
/// We use a `KeyComputer` to return a comparison key for an object.
pub trait KeyComputer<T, U: Ord> {
    ///Returns a comparison key of type *U* for comparing objects of type *T*.
    fn compute_key(&self, object: &T) -> U;
}

/// We give a default comparer for cases where comparison keys are directly the
/// objects compared.
pub struct IdentityKeyComputer();
impl<T: Clone + Ord> KeyComputer<T, T> for IdentityKeyComputer {
    fn compute_key(&self, object: &T) -> T {
        object.clone()
    }
}

/// Treap Node
pub struct RawNode<T: Display> {
    /// Real content of the Node.
    pub value: T,
    priority: u64,
    father: Option<Node<T>>,
    children: [Option<Node<T>>; 2],
}

/// Treap node (tuple struct to implement methods)
pub struct Node<T: Display>(Rc<RefCell<RawNode<T>>>);

impl<T: Display> Identifiable for RawNode<T> {}

impl<T: Display> Clone for Node<T> {
    fn clone(&self) -> Node<T> {
        Node(self.0.clone())
    }
}

impl<T: Display> Deref for Node<T> {
    type Target = Rc<RefCell<RawNode<T>>>;
    fn deref(&self) -> &Rc<RefCell<RawNode<T>>> {
        &self.0
    }
}

impl<T: Display> Node<T> {
    /// Creates a new `Node` with given value.
    fn new(value: T) -> Node<T> {
        Node(Rc::new(RefCell::new(RawNode {
            value: value,
            priority: rand::random::<u64>(),
            father: None,
            children: [None, None],
        })))
    }

    /// Returns father of given node.
    /// Do not call on sentinel node.
    fn father(&self) -> Node<T> {
        self.borrow().father.as_ref().unwrap().clone()
    }

    /// Returns an option on child in given direction.
    fn child(&self, direction: usize) -> Option<Node<T>> {
        match self.borrow().children[direction].as_ref() {
            Some(node_ref) => Some(node_ref.clone()),
            _ => None,
        }
    }

    /// Id of content (used for computing directions).
    fn id(&self) -> usize {
        self.borrow().id()
    }

    /// Returns direction of given child from self.
    /// Given child must be one of our direct children.
    fn direction_to(&self, child: &Node<T>) -> usize {
        (self.child(1).map_or(0, |c| c.id()) == child.id()) as usize
    }

    /// Link given child to us (and back).
    /// Accepts Node, None or option.
    fn set_child<U: Into<Option<Node<T>>>>(&self, direction: usize, child: U) {
        let child_option = child.into();
        if child_option.is_some() {
            child_option.as_ref()
                .unwrap()
                .borrow_mut()
                .father = Some(self.clone());
        }
        self.borrow_mut().children[direction] = child_option;
    }

    /// Rebalance the `Treap` starting from given node and going
    /// upward rotating while priorities are incorrect.
    fn balance(&self) -> Node<T> {
        let node = self.clone();
        let mut father = node.father();
        while node.borrow().priority < father.borrow().priority {
            node.rotate_upwards();
            father = node.father();
        }
        node
    }

    /// Rotate given node upwards.
    fn rotate_upwards(&self) {
        let father = self.father();
        let direction = father.direction_to(self);
        let grandfather = father.father();
        let grandfather_direction = grandfather.direction_to(&father);
        // first, replace father for grandfather
        grandfather.set_child(grandfather_direction, self.clone());
        // now exchange roles with father
        let reversed_direction = 1 - direction;
        father.set_child(direction, self.child(reversed_direction));
        self.set_child(reversed_direction, father);
    }

    /// Returns leftmost or rightmost node (can be self).
    fn extreme_node(&self, direction: usize) -> Node<T> {
        let mut node = self.clone();
        while let Some(next_node) = node.child(direction) {
            node = next_node;
        }
        node
    }

    /// Returns node just bigger or just smaller
    /// (can be None, hence the option).
    pub fn nearest_node(&self, direction: usize) -> Option<Node<T>> {
        let reversed_direction = 1 - direction;
        if let Some(child) = self.child(direction) {
            Some(child.extreme_node(reversed_direction))
        } else {
            let mut current_node = self.clone();
            let mut father = self.father();
            while father.borrow().father.is_some() {
                if father.direction_to(&current_node) == reversed_direction {
                    return Some(father);
                }
                let grandfather = father.father();
                current_node = father;
                father = grandfather;
            }
            None
        }
    }

    /// Removes ourselves from tree.
    /// Removal is safe, keeping all external node pointers valid.
    pub fn remove(&self) {
        let father = self.father();
        // first, easy cases : one or zero child
        if !self.child(1).is_some() {
            father.set_child(father.direction_to(self), self.child(0));
        } else if !self.child(0).is_some() {
            father.set_child(father.direction_to(self), self.child(1));
        } else {
            let extremum = self.child(1).unwrap().extreme_node(0);
            self.exchange_with(&extremum);
            self.remove();
        }
        // for more security disconnect pointers
        self.borrow_mut().father = None;
        self.borrow_mut().children = [None, None];
    }

    /// Exchange positions in tree between self and other.
    /// More difficult than exchanging values but ensures
    /// values always stay in the node they started in.
    /// This method DOES NOT KEEP values ordering valid.
    fn exchange_with(&self, other: &Node<T>) {
        let father = self.father();
        let other_father = other.father();
        let children = [self.child(0), self.child(1)];
        let other_children = [other.child(0), other.child(1)];
        let direction = father.direction_to(self);
        let other_direction = other_father.direction_to(other);
        father.set_child(direction, other.clone());

        for (direction, child) in other_children.iter().enumerate() {
            self.set_child(direction, child.clone());
        }
        if other_father.id() == self.id() {
            // special case: exchanging with direct child
            other.set_child(other_direction, self.clone());
            other.set_child(1 - other_direction, children[1 - other_direction].clone());
        } else {
            other_father.set_child(other_direction, self.clone());
            for (direction, child) in children.iter().enumerate() {
                other.set_child(direction, child.clone());
            }
        }
        // exchange priorities (not real treap but ok)
        let other_priority = other.borrow().priority;
        other.borrow_mut().priority = self.borrow().priority;
        self.borrow_mut().priority = other_priority;
    }

    /// Writes lines in dot (graphviz) file for displaying
    /// node and links to its children.
    fn write_dot(&self, file: &mut File) {
        let has_father = self.borrow().father.as_ref().is_some();
        let color = if has_father {
            ["red", "green"][self.father().direction_to(self)]
        } else {
            "cyan"
        };
        writeln!(file,
                 "n{}[style=filled,color={},label=\"{} / {}\"];",
                 self.id(),
                 color,
                 self.borrow().value,
                 self.borrow().priority)
            .expect("failed writing dot");

        for child in &self.borrow().children {
            if let Some(ref child_node) = *child {
                child_node.write_dot(file);
                writeln!(file, "n{} -> n{};", self.id(), child_node.id())
                    .expect("failed writing dot");
            }
        }
    }
}

/// Treap BST structure.
/// This structure is specialized for sweeping line algorithms and contains
/// the current position (used in paths comparisons).
pub struct Treap<T, U, V>
    where T: Display + Eq,
          U: Ord,
          V: KeyComputer<T, U>
{
    root: Node<T>,
    key_generator: V,
    ghost: PhantomData<U>,
}

impl<T: Display + Default + Eq, U: Ord + std::fmt::Debug, V: KeyComputer<T, U>> Treap<T, U, V> {
    /// Creates a new Treap.
    pub fn new(key_generator: V) -> Treap<T, U, V> {
        let tree = Treap {
            root: Node::new(Default::default()),
            key_generator: key_generator,
            ghost: PhantomData,
        };
        tree.root.borrow_mut().priority = 0;
        tree
    }

    /// Fills the tree with given content.
    pub fn populate<W: IntoIterator<Item = T>>(&self, content: W) {
        for value in content {
            self.add(value);
        }
    }

    /// Returns Node with given value or None.
    /// # Example
    /// ```
    /// use jimn::tree::treap::{IdentityKeyComputer, Treap};
    /// let tree = Treap::new(IdentityKeyComputer());
    /// tree.populate(1..10);
    /// let node5 = tree.find_node(5);
    /// assert!(node5.is_some());
    /// let node = node5.unwrap();
    /// assert_eq!(node.borrow().value, 5);
    /// ```
    pub fn find_node(&self, value: T) -> Option<Node<T>> {
        //let mut current_node = self.root.clone();
        let possible_start = self.root.child(1);
        if possible_start.is_some() {
            let mut current_node = possible_start.unwrap();
            let target_key = self.key_generator.compute_key(&value);
            while current_node.borrow().value != value {
                let current_key = self.key_generator.compute_key(&current_node.borrow().value);
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

    /// Adds a node to treap with given value.
    pub fn add(&self, value: T) -> Node<T> {
        let mut current_node = self.root.clone();

        let key = self.key_generator.compute_key(&value);
        let mut direction = 1; // because sentinel has min key
        while let Some(next_node) = current_node.child(direction) {
            current_node = next_node;
            let node_key = self.key_generator.compute_key(&current_node.borrow().value);
            assert!(node_key != key);
            direction = (key > node_key) as usize;
        }
        let new_node = Node::new(value);
        current_node.set_child(direction as usize, new_node.clone());
        new_node.balance()
    }

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
        Command::new("tycat")
            .arg(&png_filename)
            .status()
            .expect("tycat failed");
    }
}
