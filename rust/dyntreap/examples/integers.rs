extern crate dyntreap;
extern crate rand;
use dyntreap::{Treap, CTreap};
use rand::{Rng, StdRng};


fn main() {
    let mut treap = Treap::new();
    for x in 1..20 {
        treap.insert(x);
    }
    treap.tycat();
    let mut ctreap = CTreap::new();
    for x in 1..10 {
        ctreap.insert(x);
    }
    ctreap.tycat();
    let mut rng = StdRng::new().unwrap();
    let mut v: Vec<_> = (1..10).into_iter().collect();
    rng.shuffle(&mut v);
    for x in &v {
        println!("removing {}", x);
        ctreap.remove(x);
        ctreap.tycat();
    }
}
