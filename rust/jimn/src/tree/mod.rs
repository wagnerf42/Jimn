//! All trees structures and related functions.
pub mod treap;

use std::collections::VecDeque;
use std::fs::File;
use std::io;
use std::io::Write;
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use quadrant::Shape;
use tycat::colored_display;
use tycat::SVG_COLORS;

/// sequential counter for tycat files
static FILE_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

/// `NodeIndex` allows unique node identification.
pub type NodeIndex = usize;

/// node structure
#[derive(Debug)]
pub struct Node<T> {
    /// real content
    pub value: T,
    /// our index in the whole nodes vector
    pub index: NodeIndex,
    /// index of father node (none if we are root)
    pub father: Option<NodeIndex>,
    /// array of children indices
    pub children: Vec<NodeIndex>,
}

/// Basic tree structure
#[derive(Debug)]
pub struct Tree<T> {
    /// Vector containing all nodes.
    pub nodes: Vec<Node<T>>,
}

/// Depth first iterator on tree's values.
pub struct DepthFirstIterator<'a, T: 'a> {
    tree: &'a Tree<T>,
    remaining_nodes: Vec<NodeIndex>,
}

/// Breadth first iterator on tree's (node's level/values).
pub struct BreadthFirstIterator<'a, T: 'a> {
    tree: &'a Tree<T>,
    remaining_nodes: VecDeque<NodeIndex>,
}

impl<T: Default + Shape> Tree<T> {
    /// Create a new tree with a default root value.
    pub fn new() -> Self {
        let mut nodes = Vec::new();
        nodes.push(Node {
                       value: Default::default(),
                       index: 0,
                       father: None,
                       children: Vec::new(),
                   });
        Tree { nodes: nodes }
    }

    /// Return index for next to be added node.
    pub fn next_node_index(&self) -> NodeIndex {
        self.nodes.len()
    }

    /// Add a fatherless node.
    /// (to be re-connected later)
    pub fn add_node(&mut self, value: T) -> NodeIndex {
        let index = self.nodes.len();
        self.nodes.push(Node {
                            value: value,
                            index: index,
                            father: None,
                            children: Vec::new(),
                        });
        index
    }

    /// Sets given child as given father's.
    pub fn set_child(&mut self, father: NodeIndex, child: NodeIndex) {
        self.nodes[child].father = Some(father);
        self.nodes[father].children.push(child);
    }

    /// Return father index of given node (index).
    pub fn father(&self, id: NodeIndex) -> Option<NodeIndex> {
        self.nodes[id].father
    }

    /// Return index of root node.
    pub fn root(&self) -> NodeIndex {
        0
    }

    /// Return a depth-first iterator on all nodes.
    pub fn walk(&self) -> DepthFirstIterator<T> {
        DepthFirstIterator {
            tree: self,
            remaining_nodes: vec![0],
        }
    }

    /// Move nodes so that any parent has a lower node index than any child.
    pub fn topological_renumbering(&mut self) {
        let length = self.nodes.len();
        let mut positions: Vec<_> = (0..length).into_iter().collect(); //where is everyone
        let final_order: Vec<_> = self.walk().map(|n| n.index).collect();
        for (destination, next_node) in final_order.into_iter().enumerate() {
            //next node goes into destination
            //who should we swap with ?
            let swapped_node = self.nodes[destination].index;
            //where should we swap from ?
            let origin = positions[next_node];
            if origin != destination {
                //swap
                self.nodes.swap(origin, destination);
                //update positions
                positions.swap(next_node, swapped_node);
                //let's not forget to update index
                self.nodes[destination].index = destination;
            }
        }
    }

    /// Graphical display on console of both the tree and its contents.
    pub fn tycat(&self) -> io::Result<()> {
        colored_display(self.walk().map(|n| &n.value))?;
        let file_number = FILE_COUNT.fetch_add(1, Ordering::SeqCst);
        let dot_filename = format!("/tmp/tree-{}.dot", file_number);
        let png_filename = format!("/tmp/tree-{}.png", file_number);
        {
            let mut file = File::create(&dot_filename)?;
            writeln!(file, "digraph g {{")?;
            self.write_dot(&mut file)?;
            writeln!(file, "}}")?;
        }
        Command::new("dot").arg("-Tpng")
            .arg(&dot_filename)
            .arg("-o")
            .arg(&png_filename)
            .status()?;
        Command::new("tycat").arg(&png_filename).status()?;
        Ok(())
    }

    /// Write dot content for given node for tycat.
    /// Nodes_written is used to compute a node color matching the colors used in displaying shapes.
    fn write_dot(&self, file: &mut File) -> io::Result<()> {
        for (index, node) in self.walk().enumerate() {
            writeln!(file,
                     "n{} [color={}, style=filled];",
                     node.index,
                     SVG_COLORS[index % SVG_COLORS.len()])?;
            if let Some(father) = node.father {
                writeln!(file, "n{} -> n{}", father, node.index)?;
            }
        }
        Ok(())
    }
}

impl<'a, T: 'a> Iterator for DepthFirstIterator<'a, T> {
    type Item = &'a Node<T>;
    fn next(&mut self) -> Option<&'a Node<T>> {
        if let Some(next_index) = self.remaining_nodes.pop() {
            self.remaining_nodes.extend(&self.tree.nodes[next_index].children);
            Some(&self.tree.nodes[next_index])
        } else {
            None
        }
    }
}

impl<'a, T: 'a> Iterator for BreadthFirstIterator<'a, T> {
    type Item = &'a Node<T>;
    fn next(&mut self) -> Option<&'a Node<T>> {
        if let Some(next_index) = self.remaining_nodes.pop_front() {
            self.remaining_nodes.extend(&self.tree.nodes[next_index].children);
            Some(&self.tree.nodes[next_index])
        } else {
            None
        }
    }
}
