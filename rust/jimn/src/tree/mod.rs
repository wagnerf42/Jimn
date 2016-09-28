//! Trees used in jimn.
//! `InclusionTree` contains `Polygons` included in each others.
pub mod inclusion_tree;
use std::collections::vec_deque::VecDeque;
use point::Point;

/// compressible n-ary `Tree` for geometrical objects
#[derive(Debug)]
pub struct Tree<T> {
    /// The real stuff inside
    pub content: T,
    /// Children nodes
    pub children: Vec<Tree<T>>,
    /// This node and all its subtrees are in reality translated
    /// by all given points seen as 2D vectors.
    translations: Vec<Point>
}

impl<T> Tree<T> {
    /// Creates a new tree node with no children and given content.
    pub fn new(content: T) -> Tree<T> {
        Tree {
            content: content,
            children: Vec::new(),
            translations: vec![Point::new(0.0, 0.0)]
        }
    }
    /// Adds a child node to given node.
    pub fn add_child(&mut self, child: Tree<T>) {
        self.children.push(child);
    }
}

/// Iterator for all values stored in `Tree`, prefix, depth first.
pub struct DepthFirstIterator<'a, T: 'a> {
    stack: Vec<&'a Tree<T>>
}

/// Iterator for all values stored in `Tree`, prefix, breadth first.
pub struct BreadthFirstIterator<'a, T: 'a> {
    deque: VecDeque<&'a Tree<T>>
}

impl<'a, T> DepthFirstIterator<'a, T> {
    /// Returns on new `DepthFirstIterator` for walking given tree's values.
    /// Does not uncompress the tree.
    pub fn new(root: &Tree<T>) -> DepthFirstIterator<T> {
        DepthFirstIterator {
            stack: vec![root]
        }
    }
}

impl<'a, T> BreadthFirstIterator<'a, T> {
    /// Returns on new `BreadthFirstIterator` for walking given tree's values.
    /// Does not uncompress the tree.
    pub fn new(root: &Tree<T>) -> BreadthFirstIterator<T> {
        let mut iterator = BreadthFirstIterator {
            deque: VecDeque::new()
        };
        iterator.deque.push_back(root);
        iterator
    }
}

impl<'a, T> Iterator for DepthFirstIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        if let Some(next_node) = self.stack.pop() {
            self.stack.extend(next_node.children.iter());
            Some(&next_node.content)
        } else {
            None
        }
    }
}

impl<'a, T> Iterator for BreadthFirstIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        if let Some(next_node) = self.deque.pop_front() {
            self.deque.extend(next_node.children.iter());
            Some(&next_node.content)
        } else {
            None
        }
    }
}
