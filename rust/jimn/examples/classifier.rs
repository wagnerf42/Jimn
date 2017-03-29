extern crate jimn;

use jimn::polygon::square;
use jimn::classifier::complete_inclusion_tree;
use jimn::tree::Tree;
use jimn::tycat::colored_display;

fn main() {
    let squares = vec![square(0.0, 0.0, 10.0),
                       square(1.0, 1.0, 5.0),
                       square(6.5, 1.0, 2.0),
                       square(1.0, 7.0, 2.0),
                       square(1.5, 7.5, 0.5),
                       square(7.0, 7.0, 2.0),
                       square(7.1, 7.1, 0.5),
                       square(8.0, 8.0, 0.5)];
    colored_display(&squares).expect("display failed");
    let mut inclusion_tree = Tree::new();
    complete_inclusion_tree(&mut inclusion_tree, squares);
    inclusion_tree.tycat().expect("display failed");
}
