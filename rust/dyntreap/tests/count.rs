extern crate rand;
use rand::random;
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
