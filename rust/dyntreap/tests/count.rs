extern crate rand;
use rand::random;

extern crate dyntreap;
use dyntreap::CTreap;
use dyntreap::iterators::KeyRange;

#[test]
fn count() {
    let mut t = CTreap::new();
    for x in 0..100 {
        t.insert(x);
    }
    t.tycat();
    for _ in 0..1000 {
        let mut bounds = vec![Some(random::<usize>() % 100), Some(random::<usize>() % 100)];
        bounds.sort();
        let bounds = KeyRange { range: [bounds[0], bounds[1]] };
        assert_eq!(t.count(t.root.as_ref().unwrap(),
                           &bounds,
                           KeyRange { range: [None, None] }),
                   bounds.range[1].as_ref().unwrap() - bounds.range[0].as_ref().unwrap() + 1);
    }
}
