//! Small treap library with some dynamic keys.
//! lots of ideas taken from treap-rs (and some code)
#![feature(conservative_impl_trait)]
#![feature(collections_range)]

extern crate rand;
use rand::Rng;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::mem;
use std::iter::{once, empty, FromIterator};

use std::collections::VecDeque;
use std::collections::Bound::*;
use std::collections::range::RangeArgument;

mod counters;
use counters::{Counting, Counter, EmptyCounter};
mod node;
use node::Node;
pub mod iterators;
use iterators::{DoubleIterator, Remaining};
pub(crate) mod ranges;
use ranges::KeyRange;

pub const DECREASING: usize = 0;
pub const INCREASING: usize = 1;

pub struct RawTreap<'a, K, V, C, R>
where
    C: Counting,
    K: Ord,
    R: Rng,
{
    pub root: Option<Box<Node<V, C>>>,
    rng: R,
    keys_generator: Box<'a + Fn(&V) -> K>,
}

pub type Treap<'a, K, V> = RawTreap<'a, K, V, EmptyCounter, rand::XorShiftRng>;
pub type CTreap<'a, K, V> = RawTreap<'a, K, V, Counter, rand::XorShiftRng>;

impl<'a, V, C> RawTreap<'a, V, V, C, rand::XorShiftRng>
where
    C: Counting,
    V: Ord + Copy,
{
    pub fn new() -> RawTreap<'a, V, V, C, rand::XorShiftRng> {
        RawTreap {
            root: None,
            rng: rand::weak_rng(),
            keys_generator: Box::new(|v: &V| *v),
        }
    }
}

impl<'a, K, V, C, R> RawTreap<'a, K, V, C, R>
where
    C: Counting,
    K: Ord,
    R: Rng,
{
    /// Create a new treap with given key generator.
    pub fn new_with_key_generator<G: 'a + Fn(&V) -> K>(
        generator: G,
    ) -> RawTreap<'a, K, V, C, rand::XorShiftRng> {
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
        insert_in_subtree(
            &self.keys_generator,
            &mut self.root,
            key,
            Node::new(value, priority),
        );
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

        //let's first find the node to remove
        let mut possible_node = &mut self.root;
        loop {
            let key;
            {
                let current_node = possible_node.as_mut().unwrap();
                key = (self.keys_generator)(&current_node.value);
                if key.eq(removed_key) {
                    break;
                }
                current_node.counter = current_node.counter - Default::default();
            }
            possible_node =
                &mut { possible_node }.as_mut().unwrap().children[(key < *removed_key) as usize];
        }

        //now rotate it downwards until it becomes a leaf
        loop {
            let towards = possible_node
                .as_ref()
                .unwrap()
                .children
                .iter()
                .enumerate()
                .map(|(direction, node)| {
                    (
                        node.as_ref().map(|n| n.priority).unwrap_or(std::u64::MIN),
                        Some(direction),
                    )
                })
                .chain(once((std::u64::MIN, None)))
                .max_by_key(|&(p, _)| p)
                .unwrap()
                .1;
            if let Some(direction) = towards {
                let node = { possible_node }.as_mut().unwrap();
                node.rotate(direction);
                node.counter = node.counter - Default::default();
                possible_node = &mut node.children[1 - direction];
            } else {
                return possible_node.take().map(|n| n.value).unwrap();
            }
        }
    }
}

impl<'a, K: Ord + Copy, C: Counting> FromIterator<K> for RawTreap<'a, K, K, C, rand::XorShiftRng> {
    fn from_iter<I: IntoIterator<Item = K>>(iter: I) -> Self {
        let mut treap: RawTreap<_, _, _, _> = RawTreap::new();
        for element in iter {
            treap.insert(element);
        }
        treap
    }
}

impl<'a, K: 'a + Copy + Ord, V: 'a, C: 'a + Counting, R: 'a + Rng> RawTreap<'a, K, V, C, R> {
    /// Return iterator on all nodes, in order.
    pub fn ordered_nodes<S: RangeArgument<K>>(&'a self, range: S) -> DoubleIterator<K, V, C, R> {
        let mut remaining = VecDeque::new();
        if let Some(ref root) = self.root {
            remaining.push_back(Remaining::Subtree(root));
        }
        DoubleIterator {
            limits: KeyRange::new_from(&range),
            treap: self,
            remaining,
        }
    }

    /// Iterator through all values with keys in given range.
    pub fn ordered_values<S: RangeArgument<K>>(
        &'a self,
        range: S,
    ) -> impl DoubleEndedIterator<Item = &'a V> + 'a {
        self.ordered_nodes(range).map(|n| &n.value)
    }

    /// Iterate on all values with keys just less or just more
    /// (according to given direction) than given key.
    pub fn neighbouring_values(
        &'a self,
        key: K,
        direction: usize,
    ) -> Box<Iterator<Item = &V> + 'a> {
        let possible_neighbour = if direction == DECREASING {
            self.ordered_values((Unbounded, Excluded(key))).rev().next()
        } else {
            self.ordered_values((Excluded(key), Unbounded)).next()
        };
        if let Some(neighbour) = possible_neighbour {
            let neighbour_key = (self.keys_generator)(neighbour);
            Box::new(self.ordered_values(
                (Included(neighbour_key), Included(neighbour_key)),
            ))
        } else {
            Box::new(empty::<&V>())
        }
    }
}

fn insert_in_subtree<'a, K: Ord, V, C: Counting>(
    keys_generator: &Box<'a + Fn(&V) -> K>,
    old_node: &mut Option<Box<Node<V, C>>>,
    key: K,
    new_node: Node<V, C>,
) {
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
where
    C: Counting + Display,
    K: Ord,
    V: Display,
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
