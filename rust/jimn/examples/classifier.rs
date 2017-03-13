extern crate jimn;

use jimn::polygon::square;
use jimn::bentley_ottmann::build_inclusion_tree;
use jimn::tycat::colored_display;

fn main() {
    let squares = vec![square(0.0, 0.0, 10.0), square(1.0, 1.0, 5.0)];
    colored_display(&squares).expect("display failed");
    let inclusion_tree = build_inclusion_tree(squares);
    println!("inclusion tree is {:?}", inclusion_tree);
}
