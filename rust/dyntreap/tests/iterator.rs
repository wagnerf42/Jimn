extern crate dyntreap;
use std::collections::Bound::*;
use dyntreap::Treap;

#[test]
fn iterator() {
    let mut treap = Treap::new();
    for x in 1..1000 {
        treap.insert(x);
    }
    for (v1, v2) in treap
            .ordered_values((Excluded(400), Excluded(600)))
            .zip((401..600).into_iter()) {
        assert_eq!(*v1, v2);
    }
}
