//! Small treap library with some dynamic keys.
//! lots of ideas taken from treap-rs (and some code)
extern crate rand;
use rand::Rng;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::iter::once;
use std::mem;

mod counters;
use counters::{Counting, Counter, EmptyCounter};
mod node;
use node::Node;


pub struct RawTreap<'a, K, V, C, Rng = rand::XorShiftRng>
    where C: Counting,
          K: Ord
{
    root: Option<Box<Node<V, C>>>,
    rng: Rng,
    keys_generator: Box<'a + Fn(&V) -> K>,
}

pub type Treap<'a, K, V> = RawTreap<'a, K, V, EmptyCounter>;
pub type CTreap<'a, K, V> = RawTreap<'a, K, V, Counter>;

impl<'a, V, C> RawTreap<'a, V, V, C, rand::XorShiftRng>
    where C: Counting,
          V: Ord + Copy
{
    pub fn new() -> RawTreap<'a, V, V, C, rand::XorShiftRng> {
        RawTreap {
            root: None,
            rng: rand::weak_rng(),
            keys_generator: Box::new(|v: &V| *v),
        }
    }
}

impl<'a, K, V, C> RawTreap<'a, K, V, C, rand::XorShiftRng>
    where C: Counting,
          K: Ord
{
    /// Create a new treap with given key generator.
    pub fn new_with_key_generator<G: 'a + Fn(&V) -> K>
        (generator: G)
         -> RawTreap<'a, K, V, C, rand::XorShiftRng> {
        RawTreap {
            root: None,
            rng: rand::weak_rng(),
            keys_generator: Box::new(generator),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }

    /// Insert given value in tree.
    pub fn insert(&mut self, value: V) {
        let priority = self.rng.next_u64();
        let key = (self.keys_generator)(&value);
        insert_in_subtree(&self.keys_generator,
                          &mut self.root,
                          key,
                          Node::new(value, priority));
    }

    /// Return first value with given key.
    pub fn get(&self, searched_key: &K) -> Option<&V> {
        let mut possible_node = &self.root;
        while let Some(ref current_node) = *possible_node {
            let current_key = (self.keys_generator)(&current_node.value);
            if current_key == *searched_key {
                return Some(&current_node.value);
            }
            possible_node = &current_node.children[(current_key < *searched_key) as usize];
        }
        None
    }

    /// Remove first encountered value with given key.
    /// pre-condition : we contain the key.
    pub fn remove(&mut self, removed_key: &K) -> V {
        // we need to contain the key in order to update counters while going down.

        // i tried to do it iteratively but the borrow checker prevents me
        // from writing : possible_node = &mut current_node.children[direction]
        // :-(
        recursive_removal(&self.keys_generator, &mut self.root, removed_key)
    }
}

fn recursive_removal<'a, K: Ord, V, C: Counting>(generator: &Box<'a + Fn(&V) -> K>,
                                                 possible_node: &mut Option<Box<Node<V, C>>>,
                                                 removed_key: &K)
                                                 -> V {
    if let Some(ref mut current_node) = *possible_node {
        let current_key = (generator)(&current_node.value);
        if current_key != *removed_key {
            current_node.counter = current_node.counter - Default::default();
            return recursive_removal(generator,
                                     &mut current_node.children[(current_key < *removed_key) as
                                                                usize],
                                     removed_key);
        }
    } else {
        panic!("trying to remove a key we do not contain");
    }
    rotate_down(possible_node)
}

/// Rotate node down until we can finally remove it.
fn rotate_down<V, C: Counting>(removed_node: &mut Option<Box<Node<V, C>>>) -> V {
    // compute where we go: left/right or stop into Option<usize>
    let status = removed_node
        .as_ref()
        .unwrap()
        .children
        .iter()
        .enumerate()
        .map(|(direction, node)| {
                 (node.as_ref().map(|n| n.priority).unwrap_or(std::u64::MIN), Some(direction))
             })
        .chain(once((std::u64::MIN, None)))
        .max_by_key(|&(p, _)| p)
        .unwrap()
        .1;
    if let Some(direction) = status {
        let node = removed_node.as_mut().unwrap();
        node.rotate(direction);
        node.counter = node.counter - Default::default();
        rotate_down(&mut node.children[1 - direction])
    } else {
        removed_node.take().map(|n| n.value).unwrap()
    }
}



fn insert_in_subtree<'a, K: Ord, V, C: Counting>(keys_generator: &Box<'a + Fn(&V) -> K>,
                                                 old_node: &mut Option<Box<Node<V, C>>>,
                                                 key: K,
                                                 new_node: Node<V, C>) {
    match *old_node {
        None => {
            mem::replace(old_node, Some(Box::new(new_node)));
        }
        Some(ref mut old) => {
            old.counter = old.counter + Default::default();
            let old_key = keys_generator(&old.value);
            let direction = (old_key < key) as usize;
            insert_in_subtree(keys_generator, &mut old.children[direction], key, new_node);
            if old.priority < old.children[direction].as_ref().unwrap().priority {
                old.rotate(direction);
            }
        }
    }
}


// display functions for terminology

static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

impl<'a, K, V, C> RawTreap<'a, K, V, C, rand::XorShiftRng>
    where C: Counting + Display,
          K: Ord,
          V: Display
{
    /// Tycat display on terminal.
    pub fn tycat(&self) {
        if let Some(ref root) = self.root {
            let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
            let dot_filename = format!("/tmp/test-{}.dot", file_number);
            let png_filename = format!("/tmp/test-{}.png", file_number);
            {
                let mut file = File::create(&dot_filename).expect("cannot create dot file");
                writeln!(file, "digraph g {{").expect("failed writing dot");
                root.write_dot(&mut file);
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
}
