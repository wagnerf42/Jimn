//! All trees structures and related functions.
pub mod treap;

/// `NodeIndex` allows unique node identification.
pub type NodeIndex = usize;

/// private node structure
#[derive(Debug)]
struct Node<T> {
    value: T,
    father: Option<NodeIndex>,
    children: Vec<NodeIndex>,
}

/// Basic tree structure
#[derive(Debug)]
pub struct Tree<T> {
    nodes: Vec<Node<T>>,
}

impl<T: Default> Tree<T> {
    /// Create a new tree with a default root value.
    pub fn new() -> Self {
        let mut nodes = Vec::new();
        nodes.push(Node {
            value: Default::default(),
            father: None,
            children: Vec::new(),
        });
        Tree { nodes: nodes }
    }

    /// Add child with given value to given father.
    pub fn add_child(&mut self, value: T, father: NodeIndex) -> NodeIndex {
        self.nodes.push(Node {
            value: value,
            father: Some(father),
            children: Vec::new(),
        });
        let id = self.nodes.len() - 1;
        self.nodes[father].children.push(id);
        id
    }

    /// Return father index of given node (index). Do not call on root node.
    pub fn father(&self, id: NodeIndex) -> NodeIndex {
        self.nodes[id].father.unwrap()
    }

    /// Return index of root node.
    pub fn root(&self) -> NodeIndex {
        0
    }

    /// Graphical display on console of both the tree and its contents.
    pub fn tycat(&self) {
        unimplemented!()
    }
}
