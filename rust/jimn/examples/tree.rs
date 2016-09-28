extern crate jimn;
use jimn::tree::{Tree, DepthFirstIterator, BreadthFirstIterator};

fn main() {
    let mut t = Tree::new(5);
    t.add_child(Tree::new(3));
    t.add_child(Tree::new(2));
    t.children[0].add_child(Tree::new(0));
    println!("t is now {:?}", t);
    for value in DepthFirstIterator::new(&t) {
        println!("value is {}", value);
    }
    println!("done depth first");
    for value in BreadthFirstIterator::new(&t) {
        println!("value is {}", value);
    }
    println!("done breadth first");
}
