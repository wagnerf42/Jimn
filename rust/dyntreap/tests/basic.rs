extern crate dyntreap;
use dyntreap::Treap;

#[test]
fn insertion() {
    let treap: Treap<_, _> = (1..10).into_iter().collect();
    assert!(treap.get(&10).is_none());
    for x in 1..10 {
        let r = treap.get(&x);
        assert!(r.is_some() && *r.unwrap() == x);
    }
}
