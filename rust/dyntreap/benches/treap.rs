
#![feature(test)]
extern crate test;
extern crate rand;
extern crate dyntreap;

use rand::{Rng, StdRng};
use dyntreap::Treap;
use test::Bencher;

#[bench]
fn inserting_1000_integers(b: &mut Bencher) {
    let mut rng = StdRng::new().unwrap();
    b.iter(|| {
        let mut v: Vec<_> = (1..1000).into_iter().collect();
        rng.shuffle(&mut v);
        let mut treap = Treap::new();
        for x in &v {
            treap.insert(x);
        }
    });
}

#[bench]
fn inserting_deleting_1000_integers(b: &mut Bencher) {
    let mut rng = StdRng::new().unwrap();
    b.iter(|| {

        let mut v: Vec<_> = (1..1000).into_iter().collect();
        let mut v2: Vec<_> = v.clone();
        rng.shuffle(&mut v);
        rng.shuffle(&mut v2);
        let mut treap = Treap::new();
        for x in &v {
            treap.insert(x);
        }
        for x in &v2 {
            treap.remove(&x);
        }
    });
}
