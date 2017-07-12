extern crate dyntreap;
extern crate rand;
use std::collections::Bound::*;
use dyntreap::{Treap, CTreap};
use rand::{Rng, StdRng};


fn main() {
    let treap:Treap<_, _> = (1..20).into_iter().collect();
    treap.tycat();

    let mut ctreap: CTreap<_, _> = (1..10).into_iter().collect();
    ctreap.tycat();
    let mut rng = StdRng::new().unwrap();
    let mut v: Vec<_> = (1..10).into_iter().collect();

    println!("between 3 (excluded) and 7 (excluded)");
    for x in ctreap.ordered_values((Excluded(3), Excluded(7))) {
        println!("{}", x);
    }

    rng.shuffle(&mut v);
    for x in &v {
        println!("removing {}", x);
        ctreap.remove(x);
        ctreap.tycat();
    }
}
