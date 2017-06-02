//! Small treap library with some dynamic keys.
//! lots of ideas taken from treap-rs (and some code)
extern crate rand;
use rand::Rng;
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::process::Command;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::iter::once;

static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;


// Some subtrees-size counting genericity for treaps.
// Two variants : do or don't count.

use std::ops::{Add, Sub};

/// Do not count how many nodes in subtrees.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct EmptyCounter();

impl Display for EmptyCounter {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl Add for EmptyCounter {
    type Output = EmptyCounter;
    fn add(self, _other: EmptyCounter) -> EmptyCounter {
        EmptyCounter()
    }
}

impl Sub for EmptyCounter {
    type Output = EmptyCounter;
    fn sub(self, _other: EmptyCounter) -> EmptyCounter {
        EmptyCounter()
    }
}

impl Default for EmptyCounter {
    fn default() -> Self {
        EmptyCounter()
    }
}

/// Do count how many nodes in subtrees.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Counter(pub usize);

impl Display for Counter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "/{}", self.0)
    }
}

impl Add for Counter {
    type Output = Counter;
    fn add(self, other: Counter) -> Counter {
        Counter(self.0 + other.0)
    }
}

impl Sub for Counter {
    type Output = Counter;
    fn sub(self, other: Counter) -> Counter {
        Counter(self.0 - other.0)
    }
}

impl Default for Counter {
    fn default() -> Self {
        Counter(1)
    }
}

/// Treaps hold counters. We provide several flavors to allow choosing counting
/// subtrees sizes (with overhead) or not counting them (no overhead).
/// A counter must implement all following traits.
pub trait Counting
    : Add<Output = Self> + Sub<Output = Self> + Eq + Default + Copy {
}
impl Counting for Counter {}
impl Counting for EmptyCounter {}

/// `Identifiable` allows equivalent to python's `id`

struct Node<V, C: Counting> {
    value: V,
    priority: u64,
    counter: C,
    children: [Option<Box<Node<V, C>>>; 2],
}

pub trait Identifiable {
    /// Returns address of object which serves as unique identifier.
    fn id(&self) -> usize {
        self as *const _ as *const () as usize
    }
}

impl<V, C: Counting> Identifiable for Node<V, C> {}

impl<V, C: Counting> Node<V, C> {
    fn new(value: V, priority: u64) -> Node<V, C> {
        Node {
            value,
            priority,
            counter: Default::default(),
            children: [None, None],
        }
    }

    /// Rotate subtree. Children in given direction takes our place.
    fn rotate(&mut self, direction: usize) {
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
    fn write_dot(&self, file: &mut File) {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        let mut treap = Treap::new();
        assert!(treap.is_empty());
        for x in 1..10 {
            treap.insert(x);
        }
        assert!(treap.get(&10).is_none());
        for x in 1..10 {
            assert!(treap.get(&x).is_some());
        }
    }
}
