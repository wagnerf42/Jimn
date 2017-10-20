//! Provides a `MultiGraph` structure for path computations.
use std::collections::HashMap;
use std::hash::Hash;

use {ElementaryPath, Point, Segment};
use utils::ArrayMap;
use quadrant::{Quadrant, Shape};

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
            let edge = MultiEdge {
                vertices: ids,
                weight: length,
                multiplicity: 1,
                underlying_object: path,
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
            .map(|e| e.underlying_object.svg_string())
            .chain(
                self.vertices
                    .iter()
                    .map(|v| v.underlying_object.svg_string()),
            )
            .collect()
    }
}
