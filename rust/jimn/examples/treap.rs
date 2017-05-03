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
    println!("number of nodes larger than 5: {}",
             treap.number_of_larger_nodes(&5));
    println!("number of nodes larger than 3: {}",
             treap.number_of_larger_nodes(&3));
    println!("number of nodes larger than 7: {}",
             treap.number_of_larger_nodes(&7));
    let values: Vec<_> = treap.ordered_nodes(Some(3), Some(8)).map(|n| n.borrow().value).collect();
    println!("ordered values: {:?}", values);
}
