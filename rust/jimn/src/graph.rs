//! Provides a `MultiGraph` structure for path computations.
use std::collections::HashMap;
use std::hash::Hash;
use std::cmp::Ordering;

use {ElementaryPath, Point, Segment};
use utils::ArrayMap;
use quadrant::{Quadrant, Shape};
use tycat::display;
use std::iter::repeat;
use std::collections::BinaryHeap;

/// What do we need to be an edge in a graph ?
pub trait GraphEdge<V: Eq + Hash> {
    /// we need some ends
    fn ends(&self) -> [&V; 2];
    /// we need a length
    fn length(&self) -> f64;
}

impl GraphEdge<Point> for ElementaryPath {
    fn ends(&self) -> [&Point; 2] {
        [self.start(), self.end()]
    }
    fn length(&self) -> f64 {
        unimplemented!()
    }
}

impl GraphEdge<Point> for Segment {
    fn ends(&self) -> [&Point; 2] {
        [&self.start, &self.end]
    }
    fn length(&self) -> f64 {
        self.start.distance_to(&self.end)
    }
}

type VertexId = usize;
// I cannot manage to use references (see self referencing structs)
type EdgeId = usize;

/// a `MultiGraph` edge connecting several vertices
struct MultiEdge<'a, E: 'a> {
    vertices: [VertexId; 2],
    weight: f64,
    multiplicity: u8,
    underlying_object: &'a E,
    id: EdgeId, // needed for turning multiedge used in heap into a edgeid :-(
                //TODO: avoid it ?
}

impl<'a, E: 'a> PartialEq for MultiEdge<'a, E> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}
impl<'a, E: 'a> Eq for MultiEdge<'a, E> {}
impl<'a, E: 'a> Ord for MultiEdge<'a, E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.partial_cmp(&other.weight).unwrap()
    }
}
impl<'a, E: 'a> PartialOrd for MultiEdge<'a, E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight)
    }
}

/// a `MultiGraph` vertex
struct Vertex<'a, V: 'a> {
    neighbours: Vec<EdgeId>,
    underlying_object: &'a V,
}

/// MultiGraph structure with fast loops on neighbours
pub struct MultiGraph<'a, V: 'a, E: 'a> {
    vertices: Vec<Vertex<'a, V>>,
    edges: Vec<MultiEdge<'a, E>>,
}


impl<'a, V: Eq + Hash, E: GraphEdge<V>> MultiGraph<'a, V, E> {
    /// Create a new graph out of given paths.
    pub fn new<I: IntoIterator<Item = &'a E>>(paths: I) -> Self {
        let mut graph = MultiGraph {
            vertices: Vec::new(),
            edges: Vec::new(),
        };

        let mut all_ends = HashMap::new();
        for path in paths.into_iter() {
            let length = path.length();
            let ends = path.ends();
            let ids = ends.map(|&e| {
                *all_ends.entry(e).or_insert_with(|| {
                    let new_vertex = Vertex {
                        neighbours: Vec::new(),
                        underlying_object: e,
                    };
                    graph.vertices.push(new_vertex);
                    graph.vertices.len() - 1
                })
            });
            let edge_id = graph.edges.len();
            let edge = MultiEdge {
                vertices: ids,
                weight: length,
                multiplicity: 1,
                underlying_object: path,
                id: edge_id,
            };
            graph.edges.push(edge);
            for vertex in &ids {
                graph.vertices[*vertex]
                    .neighbours
                    .push(graph.edges.len() - 1);
            }
        }
        graph
    }
}

impl<'a, V, E> MultiGraph<'a, V, E> {
    /// Return a spanning tree of minimal weight in O(|E|log(|E|)).
    pub fn min_spanning_tree(&self) -> Vec<EdgeId> {
        let mut seen_vertices: Vec<_> = repeat(false).take(self.vertices.len()).collect();
        seen_vertices[0] = true;
        let mut remaining_edges: BinaryHeap<&MultiEdge<'a, E>> = self.vertices[0]
            .neighbours
            .iter()
            .map(|i| &self.edges[*i])
            .collect();
        let mut tree = Vec::new();
        while tree.len() != self.vertices.len() - 1 {
            let next_edge = remaining_edges
                .pop()
                .expect("not enough edges, is graph disconnected ?");
            let new_vertex = if !seen_vertices[next_edge.vertices[0]] {
                Some(next_edge.vertices[0])
            } else if !seen_vertices[next_edge.vertices[1]] {
                Some(next_edge.vertices[1])
            } else {
                None
            };
            if let Some(added_vertex) = new_vertex {
                tree.push(next_edge.id);
                remaining_edges.extend(
                    self.vertices[added_vertex]
                        .neighbours
                        .iter()
                        .map(|i| &self.edges[*i]),
                );
                seen_vertices[added_vertex] = true;
            }
        }
        tree
    }
}

impl<'a, V: Shape, E: Shape> MultiGraph<'a, V, E> {
    /// Display self and given edges on terminal.
    pub fn edges_tycat(&self, edges: &[EdgeId]) {
        let real_edges: Vec<&E> = edges
            .iter()
            .map(|&i| self.edges[i].underlying_object)
            .collect();
        display!(self, real_edges)
    }
}

impl<'a, V: Shape, E: Shape> Shape for MultiGraph<'a, V, E> {
    fn get_quadrant(&self) -> Quadrant {
        let mut quadrant = Quadrant::new(2);
        for edge in &self.edges {
            quadrant.update(&edge.underlying_object.get_quadrant());
        }
        quadrant
    }

    fn svg_string(&self) -> String {
        self.edges
            .iter()
            .map(|e| {
                e.underlying_object
                    .svg_string()
                    .repeat(e.multiplicity as usize)
            })
            .chain(
                self.vertices
                    .iter()
                    .map(|v| v.underlying_object.svg_string()),
            )
            .collect()
    }
}
