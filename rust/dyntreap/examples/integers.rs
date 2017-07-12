extern crate dyntreap;
extern crate rand;
use std::collections::Bound::*;
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

    println!("between 3 (excluded) and 7 (excluded)");
    for x in ctreap.ordered_values((Excluded(3), Excluded(7))) {
        println!("{}", x);
    }
    println!("done");

    rng.shuffle(&mut v);
    for x in &v {
        println!("removing {}", x);
        ctreap.remove(x);
        ctreap.tycat();
    }
}
