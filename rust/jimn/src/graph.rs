//! Provides a `Graph` structure for path computations.
use std::collections::HashMap;
use std::hash::Hash;
use std::cmp::Ordering;
use itertools::Itertools;

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

/// Vertices can provide shortcuts between them
pub trait GraphVertex {
    /// What is the type of the shortcut between two vertices ?
    type Path: Shape;
    /// What is the min distance between the two given vertices ?
    fn distance_to(&self, other: &Self) -> f64;
    /// What is the shortest path between the two given vertices ?
    fn shortcut(&self, other: &Self) -> Self::Path;
}

impl GraphVertex for Point {
    type Path = Segment;
    fn distance_to(&self, other: &Self) -> f64 {
        self.distance_to(other)
    }
    fn shortcut(&self, other: &Self) -> Segment {
        Segment::new(*self, *other)
    }
}

type VertexId = usize;
// I cannot manage to use references (see self referencing structs)
type EdgeId = usize;

/// a `Graph` edge connecting two vertices
struct Edge<'a, E: 'a> {
    vertices: [VertexId; 2],
    weight: f64,
    underlying_object: Option<&'a E>,
    id: EdgeId, // needed for turning edge used in heap into a edgeid :-(
                //TODO: avoid it ?
}

impl<'a, E: 'a> PartialEq for Edge<'a, E> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}
impl<'a, E: 'a> Eq for Edge<'a, E> {}
impl<'a, E: 'a> Ord for Edge<'a, E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.partial_cmp(&other.weight).unwrap().reverse() // because of max heap
    }
}
impl<'a, E: 'a> PartialOrd for Edge<'a, E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight).map(|o| o.reverse())
    }
}

/// a `Graph` vertex
struct Vertex<'a, V: 'a> {
    neighbours: Vec<EdgeId>,
    underlying_object: &'a V,
}

impl<'a, V: 'a> Vertex<'a, V> {
    fn of_odd_degree(&self) -> bool {
        self.neighbours.len() % 2 == 1
    }
}

/// Graph structure with fast loops on neighbours
pub struct Graph<'a, V: 'a + GraphVertex, E: 'a> {
    vertices: Vec<Vertex<'a, V>>,
    edges: Vec<Edge<'a, E>>,
}


impl<'a, V: Eq + Hash + GraphVertex, E: GraphEdge<V>> Graph<'a, V, E> {
    /// Create a new graph out of given paths.
    /// Assumes there is only one path between any two vertices.
    pub fn new<I: IntoIterator<Item = &'a E>>(paths: I) -> Self {
        let mut graph = Graph {
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
            let edge = Edge {
                vertices: ids,
                weight: length,
                underlying_object: Some(path),
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

impl<'a, V: GraphVertex, E> Graph<'a, V, E> {
    /// Return a spanning tree of minimal weight in O(|E|log(|E|)).
    /// This is the first step of christofides algorithm for tsp approx.
    pub fn min_spanning_tree(&self) -> Vec<EdgeId> {
        let mut seen_vertices: Vec<_> = repeat(false).take(self.vertices.len()).collect();
        seen_vertices[0] = true;
        let mut remaining_edges: BinaryHeap<&Edge<'a, E>> = self.vertices[0]
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


impl<'a, V: GraphVertex, E> Graph<'a, V, E> {
    /// Add new edges (matching) until all vertices are of even degree.
    /// We just greedily add smallest edges from complete graph.
    /// Cost is n^2 log n.
    /// This seems to be a 2approx for metric spaces.
    pub fn even_degrees(&mut self) {
        let mut choices: Vec<_> = (0..self.vertices.len())
            .into_iter()
            .combinations(2)
            .map(|v| {
                let distance = self.vertices[v[0]]
                    .underlying_object
                    .distance_to(&self.vertices[v[1]].underlying_object);
                ([v[0], v[1]], distance)
            })
            .collect();
        choices.sort_by(|c1, c2| c1.1.partial_cmp(&c2.1).unwrap());
        let mut odd_vertices_number = self.vertices.iter().filter(|v| v.of_odd_degree()).count();
        let mut remaining_choices = choices.iter();
        while odd_vertices_number != 0 {
            let &(vertices, distance) = remaining_choices.next().unwrap();
            if self.vertices[vertices[0]].of_odd_degree()
                && self.vertices[vertices[1]].of_odd_degree()
            {
                odd_vertices_number -= 2;
                let id = self.edges.len();
                self.edges.push(Edge {
                    vertices,
                    weight: distance,
                    underlying_object: None,
                    id,
                });
            }
        }
    }
}

impl<'a, V: GraphVertex + Shape, E: Shape> Graph<'a, V, E> {
    /// Display self and given edges on terminal.
    pub fn edges_tycat(&self, edges: &[EdgeId]) {
        let real_edges: Vec<&E> = edges
            .iter()
            .filter_map(|&i| self.edges[i].underlying_object)
            .collect();
        display!(self, real_edges)
    }
}

impl<'a, V: Shape + GraphVertex, E: Shape> Shape for Graph<'a, V, E> {
    fn get_quadrant(&self) -> Quadrant {
        let mut quadrant = Quadrant::new(2);
        for object in self.edges.iter().filter_map(|e| e.underlying_object) {
            quadrant.update(&object.get_quadrant());
        }
        quadrant
    }

    fn svg_string(&self) -> String {
        self.edges
            .iter()
            .map(|e| {
                e.underlying_object
                    .map(|o| o.svg_string())
                    .unwrap_or_else(|| {
                        self.vertices[e.vertices[0]]
                            .underlying_object
                            .shortcut(&self.vertices[e.vertices[1]].underlying_object)
                            .svg_string()
                    })
            })
            .chain(
                self.vertices
                    .iter()
                    .map(|v| v.underlying_object.svg_string()),
            )
            .collect()
    }
}
