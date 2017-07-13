extern crate dyntreap;
extern crate rand;

use dyntreap::Treap;
use rand::{random, Rng};

#[test]
fn insert_remove() {
    let mut elements: Vec<_> = (0..1000)
        .into_iter()
        .map(|_| random::<i32>() % 100)
        .collect();
    rand::thread_rng().shuffle(&mut elements);
    let mut treap: Treap<_, _> = elements.iter().cloned().collect();
    rand::thread_rng().shuffle(&mut elements);
    for e in &elements {
        treap.remove(e);
    }
    assert!(treap.is_empty());
}
