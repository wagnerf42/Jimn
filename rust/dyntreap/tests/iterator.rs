extern crate dyntreap;
use dyntreap::{Treap, INCREASING};

#[test]
fn iterator() {
    let mut treap = Treap::new();
    for x in 1..1000 {
        treap.insert(x);
    }
    for x in 1..1000 {
        treap.insert(x);
    }
    let mut elements = treap
        .ordered_nodes(INCREASING)
        .lower_bound(399)
        .upper_bound(600);
    //    for x in 400..600 {
    //        let next_node = elements.next();
    //        assert_eq!(x, next_node.unwrap().value);
    //        let next_node = elements.next();
    //        assert_eq!(x, next_node.unwrap().value);
    //    }
}
