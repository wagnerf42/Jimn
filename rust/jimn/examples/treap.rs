extern crate jimn;
use std::rc::Rc;
use std::cell::RefCell;
use jimn::tree::treap::{IdentityKeyComputer, CountingTreap};

fn main() {
    let treap = CountingTreap::new(Rc::new(RefCell::new(IdentityKeyComputer())));
    treap.populate(1..10);
    treap.tycat();
    println!("adding 12");
    treap.add(12);
    treap.tycat();
    println!("removing 7");
    let node7 = treap.find_node(7).unwrap();
    node7.remove();
    treap.tycat();
}
