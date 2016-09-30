//! Provides a `Treap` structure for sweeping line algorithms.
use std::cell::RefCell;
use std::rc::Rc;
use std::ops::Deref;
use std::fs::File;
use std::process::Command;
use std::io::prelude::*;
use utils::Identifiable;
use rand;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

/// sequential counter for tycat files
static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;
//TODO:
//genericization (one type for values, one type for keys ?)

/// Treap Node
pub struct RawNode {
    /// Real content of the Node.
    pub value: u32,
    priority: u64,
    father: Option<Node>,
    children: [Option<Node>; 2]
}

/// Treap node (tuple struct to implement methods)
pub struct Node(Rc<RefCell<RawNode>>);

impl Identifiable for RawNode {}

impl Clone for Node {
    fn clone(&self) -> Node {
        Node(self.0.clone())
    }
}

impl Deref for Node {
    type Target = Rc<RefCell<RawNode>>;
    fn deref(&self) -> &Rc<RefCell<RawNode>> {
        &self.0
    }
}

impl Node {
    /// Creates a new `Node` with given value.
    fn new(value: u32) -> Node {
        Node(Rc::new(RefCell::new(RawNode {
            value: value,
            priority: rand::random::<u64>(),
            father: None,
            children: [None, None]
        })))
    }

    /// Returns father of given node.
    /// Do not call on sentinel node.
    fn father(&self) -> Node {
        self.borrow().father.as_ref().unwrap().clone()
    }

    /// Returns an option on child in given direction.
    fn child(&self, direction: usize) -> Option<Node> {
        match self.borrow().children[direction].as_ref() {
            Some(node_ref) => Some(node_ref.clone()),
            _ => None
        }
    }

    /// Id of content (used for computing directions).
    fn id(&self) -> usize {
        self.borrow().id()
    }

    /// Returns direction of given child from self.
    /// Given child must be one of our direct children.
    fn direction_to(&self, child: &Node) -> usize {
        (self.child(1).map_or(0, |c| c.id()) == child.id()) as usize
    }

    /// Link given child to us (and back).
    /// Accepts Node, None or option.
    fn set_child<T: Into<Option<Node>>>(&self,
                                        direction: usize,
                                        child: T) {
        let child_option = child.into();
        if child_option.is_some() {
            child_option.as_ref().unwrap().borrow_mut()
                .father = Some(self.clone());
        }
        self.borrow_mut().children[direction] = child_option;
    }

    /// Rebalance the `Treap` starting from given node and going
    /// upward rotating while priorities are incorrect.
    fn balance(&self) -> Node {
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
    fn extreme_node(&self, direction: usize) -> Node {
        let mut node = self.clone();
        while let Some(next_node) = node.child(direction) {
            node = next_node;
        }
        node
    }

    /// Returns node just bigger or just smaller
    /// (can be None, hence the option).
    pub fn nearest_node(&self, direction: usize) -> Option<Node> {
        let reversed_direction = 1-direction;
        if let Some(child) = self.child(direction) {
            return Some(child.extreme_node(reversed_direction));
        } else {
            let mut current_node = self.clone();
            let mut father = self.father();
            while father.borrow().value != 0 {
                if father.direction_to(&current_node) == reversed_direction {
                    return Some(father);
                }
                let grandfather = father.father();
                current_node = father;
                father = grandfather;
            }
            return None;
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
    fn exchange_with(&self, other: &Node) {
        let father = self.father();
        let other_father = other.father();
        let children = [self.child(0), self.child(1)];
        let other_children = [other.child(0), other.child(1)];
        let direction = father.direction_to(self);
        let other_direction = other_father.direction_to(other);
        father.set_child(direction, other.clone());
        
        for direction in 0..2 {
            self.set_child(direction, other_children[direction].clone());
        }
        if other_father.id() == self.id() {
            // special case: exchanging with direct child
            other.set_child(other_direction, self.clone());
            other.set_child(1-other_direction,
                            children[1-other_direction].clone());
        } else {
            other_father.set_child(other_direction, self.clone());
            for direction in 0..2 {
                other.set_child(direction, children[direction].clone());
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
        writeln!(file, "n{}[label=\"{} / {}\"];",
                 self.id(),
                 self.borrow().value,
                 self.borrow().priority)
            .expect("failed writing dot");

        for child in &self.borrow().children {
            if let &Some(ref child_node) = child {
                child_node.write_dot(file);
                writeln!(file, "n{} -> n{};",
                         self.id(),
                         child_node.id())
                    .expect("failed writing dot");
            }
        }
    }
}

/// Treap BST structure.
/// This structure is specialized for sweeping line algorithms and contains
/// the current position (used in paths comparisons).
pub struct Treap {
    root: Node,
    current_point: u32
}

impl Treap {
    /// Creates a new Treap.
    pub fn new() -> Treap {
        let tree = Treap {
            root: Node::new(0),
            current_point: 0
        };
        tree.root.borrow_mut().priority = 0;
        tree
    }

    /// Fills the tree with given content.
    pub fn populate<T: IntoIterator<Item=u32>> (&self, content: T) {
        for value in content {
            self.add(value);
        }
    }

    /// Returns Node with given value or None.
    /// # Example
    /// ```
    /// use jimn::tree::treap::Treap;
    /// let tree = Treap::new();
    /// tree.populate(1..10);
    /// let node5 = tree.find_node(5);
    /// assert!(node5.is_some());
    /// let node = node5.unwrap();
    /// assert_eq!(node.borrow().value, 5);
    /// ```
    pub fn find_node(&self, value: u32) -> Option<Node> {
        let mut current_node = self.root.clone();
        let target_key = self.comparison_key(value);
        while current_node.borrow().value != value {
            let current_key = self.comparison_key(current_node.borrow().value);
            let direction = (target_key > current_key) as usize;
            if let Some(next_node) = current_node.child(direction) {
                current_node = next_node;
            } else {
                return None
            }
        }
        Some(current_node)
    }

    /// Adds a node to treap with given value.
    pub fn add(&self, value: u32) -> Node {
        let mut current_node = self.root.clone();

        let key = self.comparison_key(value);
        let mut direction = 1; // because sentinel has min key
        while let Some(next_node) = current_node.child(direction) {
            current_node = next_node;
            let node_key = self.comparison_key(
                current_node.borrow().value);
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
            let mut file = File::create(&dot_filename)
                .expect("cannot create dot file");
            writeln!(file, "digraph g {{")
                .expect("failed writing dot");
            self.root.write_dot(&mut file);
            writeln!(file, "}}")
                .expect("failed writing dot");
        }
        Command::new("dot").arg("-Tpng").arg(&dot_filename)
            .arg("-o").arg(&png_filename)
            .status().expect("dot failed");
        Command::new("tycat").arg(&png_filename)
            .status().expect("tycat failed");
    }
    /// Get key from value to compare nodes.
    /// Uses current position.
    fn comparison_key(&self, value: u32) -> f64 {
        (value + self.current_point) as f64
    }
}
