extern crate dyntreap;
use dyntreap::Treap;

#[test]
fn insertion() {
    let mut treap = Treap::new();
    assert!(treap.is_empty());
    for x in 1..10 {
        treap.insert(x);
    }
    assert!(treap.get(&10).is_none());
    for x in 1..10 {
        let r = treap.get(&x);
        assert!(r.is_some() && *r.unwrap() == x);
    }
}
