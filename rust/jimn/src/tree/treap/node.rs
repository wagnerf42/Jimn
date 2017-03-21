//! Treap Node
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::fs::File;
use std::ops::Deref;
use std::fmt::Display;
use std::io::prelude::*;
use rand;
use super::Counting;
use utils::Identifiable;

/// Treap Node
pub struct RawNode<T, U: Counting> {
    /// Real content of the Node.
    pub value: T,
    /// Priority in treap.
    pub priority: u64,
    father: Option<Weak<RefCell<RawNode<T, U>>>>,
    children: [Option<Node<T, U>>; 2],
    /// Number of nodes in subtree (or not).
    pub counter: U,
}

/// Treap node (tuple struct to implement methods)
pub struct Node<T, U: Counting>(Rc<RefCell<RawNode<T, U>>>);

impl<T, U: Counting> Identifiable for RawNode<T, U> {}

impl<T, U: Counting> Clone for Node<T, U> {
    fn clone(&self) -> Node<T, U> {
        Node(self.0.clone())
    }
}

impl<T, U: Counting> Deref for Node<T, U> {
    type Target = Rc<RefCell<RawNode<T, U>>>;
    fn deref(&self) -> &Rc<RefCell<RawNode<T, U>>> {
        &self.0
    }
}

impl<T, U: Counting> Node<T, U> {
    /// Creates a new `Node` with given value.
    pub fn new(value: T) -> Node<T, U> {
        Node(Rc::new(RefCell::new(RawNode {
                                      value: value,
                                      priority: rand::random::<u64>(),
                                      father: None,
                                      children: [None, None],
                                      counter: Default::default(),
                                  })))
    }

    /// Return if we are sentinel node.
    pub fn is_root(&self) -> bool {
        self.borrow().father.is_none()
    }

    /// Returns father of given node.
    /// Do not call on sentinel node.
    pub fn father(&self) -> Node<T, U> {
        Node(self.borrow()
                 .father
                 .as_ref()
                 .unwrap()
                 .upgrade()
                 .unwrap())
    }

    /// Returns an option on child in given direction.
    pub fn child(&self, direction: usize) -> Option<Node<T, U>> {
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
    fn direction_to(&self, child: &Node<T, U>) -> usize {
        (self.child(1).map_or(0, |c| c.id()) == child.id()) as usize
    }

    /// Link given child to us (and back).
    /// Accepts Node, None or option.
    fn set_child<V: Into<Option<Node<T, U>>>>(&self, direction: usize, child: V) {
        let child_option = child.into();
        if child_option.is_some() {
            child_option.as_ref()
                .unwrap()
                .borrow_mut()
                .father = Some(Rc::downgrade(&self.0));
        }
        self.borrow_mut().children[direction] = child_option;
    }

    /// Rebalance the `Treap` starting from given node and going
    /// upward rotating while priorities are incorrect.
    fn balance(&self) -> Node<T, U> {
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

        // start by updating counters
        self.borrow_mut().counter = father.borrow().counter;
        let delta = if let Some(ref child) = self.child(direction) {
            child.borrow().counter + Default::default()
        } else {
            Default::default()
        };
        let old_counter = father.borrow().counter;
        father.borrow_mut().counter = old_counter - delta;


        // now, replace father for grandfather
        grandfather.set_child(grandfather_direction, self.clone());

        // finally exchange roles with father
        let reversed_direction = 1 - direction;
        father.set_child(direction, self.child(reversed_direction));
        self.set_child(reversed_direction, father);
    }

    /// Returns leftmost or rightmost node (can be self).
    fn extreme_node(&self, direction: usize) -> Node<T, U> {
        let mut node = self.clone();
        while let Some(next_node) = node.child(direction) {
            node = next_node;
        }
        node
    }

    /// Returns node just bigger or just smaller
    /// (can be None, hence the option).
    pub fn nearest_node(&self, direction: usize) -> Option<Node<T, U>> {
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
            father.update_counters(Default::default(), false);
        } else if !self.child(0).is_some() {
            father.set_child(father.direction_to(self), self.child(1));
            father.update_counters(Default::default(), false);
        } else {
            let extremum = self.child(1).unwrap().extreme_node(0);
            self.exchange_with(&extremum);
            self.remove();
            return;
        }
        // for more security disconnect pointers
        self.borrow_mut().father = None;
        self.borrow_mut().children = [None, None];
    }

    /// Exchange positions in tree between self and other.
    /// More difficult than exchanging values but ensures
    /// values always stay in the node they started in.
    /// This method DOES NOT KEEP values ordering valid.
    fn exchange_with(&self, other: &Node<T, U>) {
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


    /// Update counters by adding/removing given delta to/from all ancestors.
    /// O(h) for CountingTreap
    /// O(1) for Treap
    fn update_counters(&self, delta: U, add: bool) {
        let mut node = self.clone();
        loop {
            let old_counter = node.borrow().counter;
            let new_counter = if add {
                old_counter + delta
            } else {
                old_counter - delta
            };
            if old_counter == new_counter {
                return;
            }
            node.borrow_mut().counter = new_counter;
            if node.is_root() {
                return;
            }
            node = node.father();
        }
    }

    /// Create new child node at given direction with given value and rebalance Treap.
    pub fn add_child_with_value(&mut self, direction: usize, value: T) -> Node<T, U> {
        let new_node = Node::new(value);
        self.set_child(direction, new_node.clone());
        self.update_counters(Default::default(), true);
        new_node.balance()
    }
}

impl<T: Display, U: Counting> Node<T, U> {
    /// Writes lines in dot (graphviz) file for displaying
    /// node and links to its children.
    pub fn write_dot(&self, file: &mut File) {
        let has_father = self.borrow()
            .father
            .as_ref()
            .is_some();
        let color = if has_father {
            ["red", "green"][self.father().direction_to(self)]
        } else {
            "cyan"
        };
        writeln!(file,
                 "n{}[style=filled,color={},label=\"{}\"];",
                 self.id(),
                 color,
                 self.borrow().value)
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
