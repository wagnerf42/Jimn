extern crate rand;
use rand::{random, Rng};
use std::collections::Bound::*;

extern crate dyntreap;
use dyntreap::CTreap;

#[test]
fn count() {
    for _ in 0..1000 {
        let elements: Vec<_> = (0..100).into_iter().map(|_| random::<i32>() % 10).collect();
        let t: CTreap<_, _> = elements.iter().cloned().collect();
        let mut bounds = vec![random::<i32>() % 10, random::<i32>() % 10];
        bounds.sort();
        let lower_bound = Included(bounds[0]);
        let upper_bound = Included(bounds[1]);

        let iterator_count = t.ordered_nodes((lower_bound, upper_bound)).len();
        let manual_count = elements
            .iter()
            .filter(|&e| *e >= bounds[0] && *e <= bounds[1])
            .count();
        assert_eq!(iterator_count, manual_count);
    }
}

#[test]
fn dynamic_count() {
    let mut elements: Vec<_> = (0..100).into_iter().map(|i| i % 10).collect();
    rand::thread_rng().shuffle(&mut elements);
    let mut ctreap: CTreap<_, _> = elements.iter().cloned().collect();
    for count in (0..10).into_iter().rev() {
        for digit in 0..10 {
            ctreap.remove(&digit);
            assert_eq!(
                count,
                ctreap
                    .ordered_nodes((Included(digit), Included(digit)))
                    .len()
            );
        }
    }
}
