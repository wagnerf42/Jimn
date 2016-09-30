extern crate jimn;
use jimn::tree::treap::Treap;

fn main() {
    let tree = Treap::new();
    println!("1 to 10:");
    tree.populate(1..11);
    tree.tycat();
    println!("removed 5:");
    let n5 = tree.find_node(5).unwrap();
    n5.remove();
    tree.tycat();
}
