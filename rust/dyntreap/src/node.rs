use super::counters::Counting;
use std::fs::File;
use std::io::Write;
use std::fmt::Display;
use std::mem;

pub struct Node<V, C: Counting> {
    pub value: V,
    pub priority: u64,
    pub counter: C,
    pub children: [Option<Box<Node<V, C>>>; 2],
}

/// `Identifiable` allows equivalent to python's `id`
pub trait Identifiable {
    /// Returns address of object which serves as unique identifier.
    fn id(&self) -> usize {
        self as *const _ as *const () as usize
    }
}

impl<V, C: Counting> Identifiable for Node<V, C> {}

impl<V, C: Counting> Node<V, C> {
    pub fn new(value: V, priority: u64) -> Node<V, C> {
        Node {
            value,
            priority,
            counter: Default::default(),
            children: [None, None],
        }
    }

    /// Rotate subtree. Children in given direction takes our place.
    pub fn rotate(&mut self, direction: usize) {
        // cut subtree away from old root
        let subtree = mem::replace(&mut self.children[direction], None);
        if let Some(mut new_root) = subtree {
            // exchange place with old root
            mem::swap(self, &mut new_root);
            // also exchange variable names
            let (mut old_root, new_root) = (new_root, self);
            new_root.counter = old_root.counter;

            // displace new root's subtree
            mem::swap(&mut new_root.children[1 - direction],
                      &mut old_root.children[direction]);

            // update old root counter
            let mut counter = Default::default();
            for potential_child in &old_root.children {
                if let Some(ref child) = *potential_child {
                    counter = counter + child.counter;
                }
            }
            old_root.counter = counter;

            // old root should be our child now
            mem::replace(&mut new_root.children[1 - direction], Some(old_root));

        } else {
            panic!("rebalancing on empty child");
        }
    }
}

const ARROWS_COLORS: [&str; 2] = ["red", "blue"];

impl<V: Display, C: Counting + Display> Node<V, C> {
    pub fn write_dot(&self, file: &mut File) {
        writeln!(file,
                 "n{}[label=\"{}{}\"];",
                 self.id(),
                 self.value,
                 self.counter)
            .expect("failed writing dot");

        for (index, child) in self.children.iter().enumerate() {
            if let Some(ref child_node) = *child {
                child_node.write_dot(file);
                writeln!(file,
                         "n{} -> n{}[color=\"{}\"];",
                         self.id(),
                         child_node.id(),
                         ARROWS_COLORS[index])
                    .expect("failed writing dot");
            }
        }
    }
}
