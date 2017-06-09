extern crate dyntreap;
use dyntreap::{Treap, INCREASING};

#[test]
fn iterator() {
    let mut treap = Treap::new();
    for x in 1..1000 {
        treap.insert(x);
    }
    let mut elements = treap
        .ordered_nodes(INCREASING)
        .lower_bound(400)
        .upper_bound(600);
    for x in 400..601 {
        let node = elements.next();
        assert!(node.is_some());
        assert_eq!(node.unwrap().value, x)
    }
}
