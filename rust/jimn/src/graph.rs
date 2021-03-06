//! Provides a `Graph` structure for path computations.
use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use itertools::Itertools;

use std::collections::BinaryHeap;
use std::iter::repeat;
use disjoint_sets::UnionFind;

use {ElementaryPath, Point, Segment, TaggedPath};
use utils::ArrayMap;
use utils::coordinates_hash::SquareHash;
use quadrant::{Quadrant, Shape};

type VertexId = usize;
// I cannot manage to use references (see self referencing structs)
type EdgeId = usize;

/// a `Graph` edge connecting two vertices
struct Edge<'a> {
    vertices: [VertexId; 2],
    weight: f64,
    underlying_object: Option<&'a TaggedPath>,
    id: EdgeId, // needed for turning edge used in heap into a edgeid :-(
                //TODO: avoid it ?
}

impl<'a> PartialEq for Edge<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}
impl<'a> Eq for Edge<'a> {}
impl<'a> Ord for Edge<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.partial_cmp(&other.weight).unwrap().reverse() // because of max heap
    }
}
impl<'a> PartialOrd for Edge<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight).map(|o| o.reverse())
    }
}

/// a `Graph` vertex
struct Vertex<'a> {
    neighbours: Vec<EdgeId>,
    underlying_object: &'a Point,
}

impl<'a> Vertex<'a> {
    fn of_odd_degree(&self) -> bool {
        self.neighbours.len() % 2 == 1
    }
}

/// Graph structure with fast loops on neighbours
pub struct Graph<'a> {
    vertices: Vec<Vertex<'a>>,
    edges: Vec<Edge<'a>>,
}

impl<'a> Graph<'a> {
    /// Create a new graph out of given paths.
    /// Assumes there is only one path between any two vertices.
    pub fn new<I: IntoIterator<Item = &'a TaggedPath>>(paths: I) -> Self {
        let mut graph = Graph {
            vertices: Vec::new(),
            edges: Vec::new(),
        };

        let mut all_ends = HashMap::new();
        for path in paths {
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

    /// Return a spanning tree of minimal weight in O(|E|log(|E|)).
    /// This is the first step of christofides algorithm for tsp approx.
    pub fn min_spanning_tree(&self) -> Vec<EdgeId> {
        let mut seen_vertices: Vec<_> = repeat(false).take(self.vertices.len()).collect();
        seen_vertices[0] = true;
        let mut remaining_edges: BinaryHeap<&Edge<'a>> = self.vertices[0]
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

    /// Return eulerian cycle of real underlying paths.
    /// Pre-condition: all vertices are of even degrees.
    /// Cost is in O(n+m*degree).
    pub fn eulerian_cycle(&self) -> Vec<TaggedPath> {
        if cfg!(debug_assertions) {
            for vertex in &self.vertices {
                assert!(!vertex.of_odd_degree());
            }
        }
        let mut cycles = HashMap::new();
        let mut remaining_edges: HashSet<_> = self.edges.iter().map(|e| e.id).collect();
        // remember vertices we could use as a starting point
        // cycles starting points are constrained to be on a previously discovered cycle
        // this way we can rebuild the final cycle easily
        let mut possible_starts = HashMap::with_capacity(self.vertices.len());
        // we can start with vertex 0
        possible_starts.insert(0, self.vertices[0].neighbours.len());
        while !possible_starts.is_empty() {
            // let's find a new cycle
            let starting_vertex = *possible_starts.keys().next().unwrap();

            let mut current_cycle = Vec::new();
            let mut current_vertex = starting_vertex;

            loop {
                let current_edge = *self.vertices[current_vertex]
                    .neighbours
                    .iter()
                    .find(|e| remaining_edges.contains(e))
                    .unwrap();
                // store path and update all structs
                for vertex in &self.edges[current_edge].vertices {
                    let remove = {
                        let remaining_degree =
                            possible_starts.entry(*vertex).or_insert_with(|| {
                                self.vertices[*vertex]
                                    .neighbours
                                    .iter()
                                    .filter(|n| remaining_edges.contains(n))
                                    .count() // this loop inccurs the extra "degree" cost
                            });
                        *remaining_degree -= 1;
                        *remaining_degree == 0
                    };
                    if remove {
                        possible_starts.remove(vertex);
                    }
                }
                remaining_edges.remove(&current_edge);
                current_cycle.push(current_edge);

                current_vertex = *self.edges[current_edge]
                    .vertices
                    .iter()
                    .find(|v| !current_vertex.eq(v))
                    .unwrap();

                if current_vertex == starting_vertex {
                    break;
                }
            }
            cycles
                .entry(starting_vertex)
                .or_insert_with(Vec::new)
                .extend(current_cycle); // if two cycles start at same vertex we just concatenate
        }

        // now assemble all cycles together (follow first one) and replace by real paths
        let first_vertex = 0;
        let first_cycle = cycles.remove(&first_vertex).unwrap();
        let mut paths = Vec::new(); // final result
        self.follow_cycle(first_vertex, &first_cycle, &mut cycles, &mut paths);
        assert!(cycles.is_empty());
        paths
    }

    // recursively reconstruct full eulerian cycle
    fn follow_cycle(
        &self,
        first_vertex: VertexId,
        cycle: &[EdgeId],
        cycles: &mut HashMap<VertexId, Vec<EdgeId>>,
        paths: &mut Vec<TaggedPath>,
    ) {
        let mut current_vertex = first_vertex;
        for next_edge in cycle {
            // first, try to reconnect a cycle here
            if let Some(sub_cycle) = cycles.remove(&current_vertex) {
                self.follow_cycle(current_vertex, &sub_cycle, cycles, paths);
            }
            // now, go on with our own edge
            paths.push(self.real_path_for(*next_edge));
            current_vertex = *self.edges[*next_edge]
                .vertices
                .iter()
                .find(|v| !current_vertex.eq(v))
                .unwrap();
        }
    }

    fn real_path_for(&self, edge: EdgeId) -> TaggedPath {
        self.edges[edge]
            .underlying_object
            .cloned()
            .unwrap_or_else(|| {
                TaggedPath::Move(ElementaryPath::Segment(Segment::new(
                    *self.vertices[self.edges[edge].vertices[0]].underlying_object,
                    *self.vertices[self.edges[edge].vertices[1]].underlying_object,
                )))
            })
    }

    /// Add new edges (matching) until all vertices are of even degree.
    /// We just greedily add smallest edges from complete graph.
    /// Cost is n^2 log n.
    /// This seems to be a 2approx for metric spaces.
    /// Returns the total added weights.
    pub fn even_degrees(&mut self) -> f64 {
        let mut choices: Vec<_> = (0..self.vertices.len())
            .into_iter()
            .combinations(2)
            .map(|v| {
                let distance = self.vertices[v[0]]
                    .underlying_object
                    .distance_to(self.vertices[v[1]].underlying_object);
                ([v[0], v[1]], distance)
            })
            .collect();
        choices.sort_by(|c1, c2| c1.1.partial_cmp(&c2.1).unwrap());
        let mut odd_vertices_number = self.vertices.iter().filter(|v| v.of_odd_degree()).count();
        let mut remaining_choices = choices.iter();
        let mut total_added_weight = 0.0;
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
                total_added_weight += distance;
                self.vertices[vertices[0]].neighbours.push(id);
                self.vertices[vertices[1]].neighbours.push(id);
            }
        }
        total_added_weight
    }

    /// Display self and given edges on terminal.
    pub fn edges_tycat(&self, edges: &[EdgeId]) {
        let real_edges: Vec<&TaggedPath> = edges
            .iter()
            .filter_map(|&i| self.edges[i].underlying_object)
            .collect();
        display!(self, unicolor!(&real_edges))
    }
    /// Display self and given vertices on terminal.
    pub fn vertices_tycat(&self, vertices: &[VertexId]) {
        let real_vertices: Vec<&Point> = vertices
            .iter()
            .map(|&i| self.vertices[i].underlying_object)
            .collect();
        display!(self, unicolor!(&real_vertices))
    }

    /// Return groups of nearby vertices starting with very near groups.
    /// groups grow larger and larger until last group containing all vertices.
    pub fn nearby_vertices(&self) -> Vec<Vec<VertexId>> {
        let mut starting_quadrant = Quadrant::new(2);
        for vertex in &self.vertices {
            starting_quadrant.update(&vertex.underlying_object.get_quadrant());
        }
        let starting_precision = starting_quadrant.size() * 2.0;
        let mut colliding_vertices = Vec::new();
        let mut precision = starting_precision;
        loop {
            let mut hash = SquareHash::new(precision); //TODO: drain
            for (vertex_id, vertex) in self.vertices.iter().enumerate() {
                hash.hash_point(vertex.underlying_object, vertex_id);
            }
            let old_size = colliding_vertices.len();
            colliding_vertices.extend(
                hash.hashes
                    .iter()
                    .flat_map(|h| h.values().filter(|p| p.len() > 1).cloned()),
                //TODO: drain
            );
            let new_size = colliding_vertices.len();
            if new_size == old_size {
                break;
            }
            precision /= 2.0;
        }
        colliding_vertices
    }

    /// Add edges until all degrees are even.
    /// Asymptotic in O(n log(s)) where s is ratio between max and min distance.
    /// Worse results quality than O(n^2) algorithm it seems to be a 4sqrt(2) approx.
    /// 2 from the greedy algorithm and 2sqrt(2) from the hashing in squares.
    /// Return total weight of added edges.
    pub fn fast_even_degrees(&mut self, nearby_vertices: &[Vec<VertexId>]) -> f64 {
        let mut odd_vertices_number = self.vertices.iter().filter(|v| v.of_odd_degree()).count();
        let mut total_added_weight = 0.0;
        for group in nearby_vertices.iter().rev() {
            let mut unused_vertex = None;
            for vertex in group {
                if self.vertices[*vertex].of_odd_degree() {
                    unused_vertex = if let Some(start) = unused_vertex {
                        let end = *vertex;
                        odd_vertices_number -= 2;
                        let id = self.edges.len();
                        let distance = {
                            let v: &Vertex = &self.vertices[start];
                            let p: &Point = v.underlying_object;
                            p.distance_to(self.vertices[end].underlying_object)
                        };
                        total_added_weight += distance;
                        self.edges.push(Edge {
                            vertices: [start, end],
                            weight: distance,
                            underlying_object: None,
                            id,
                        });
                        self.vertices[start].neighbours.push(id);
                        self.vertices[end].neighbours.push(id);
                        if odd_vertices_number == 0 {
                            return total_added_weight;
                        }
                        None
                    } else {
                        Some(*vertex)
                    };
                }
            }
        }
        panic!("failed even degrees")
    }

    /// Reconnect graph into one piece very fast.
    /// We use Kruskal's algorithm modified to take nearby vertices into account.
    pub fn reconnect(&mut self, nearby_vertices: &[Vec<VertexId>]) -> f64 {
        let mut components = UnionFind::new(self.vertices.len());
        let mut remaining_parts = self.vertices.len();
        let mut total_added_weight = 0.0;
        for edge in &self.edges {
            if components.union(edge.vertices[0], edge.vertices[1]) {
                remaining_parts -= 1;
            }
        }
        if remaining_parts == 1 {
            return total_added_weight;
        }
        for group in nearby_vertices.iter().rev() {
            let map: HashMap<_, _> = group
                .iter()
                .cloned()
                .map(|v| (components.find(v), v))
                .collect();
            for (start, end) in map.values().cloned().tuple_windows() {
                components.union(start, end);
                remaining_parts -= 1;

                let id = self.edges.len();
                let distance = {
                    let v: &Vertex = &self.vertices[start];
                    let p: &Point = v.underlying_object;
                    p.distance_to(self.vertices[end].underlying_object)
                };
                total_added_weight += distance;
                self.edges.push(Edge {
                    vertices: [start, end],
                    weight: distance,
                    underlying_object: None,
                    id,
                });
                self.vertices[start].neighbours.push(id);
                self.vertices[end].neighbours.push(id);

                if remaining_parts == 1 {
                    return total_added_weight;
                }
            }
        }
        panic!("cannot reconnect graph");
    }
}

impl<'a> Shape for Graph<'a> {
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
            .map(|e| self.real_path_for(e.id).svg_string())
//            .chain(
//                self.vertices
//                    .iter()
//                    .map(|v| v.underlying_object.svg_string()),
//            )
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use {Point, Polygon};
    use utils::coordinates_hash::PointsHash;
    use tile::hexagonal_tile;
    use clipper::clip;

    #[bench]
    fn graph_quadratic_even_degrees(b: &mut Bencher) {
        b.iter(|| {
            let points = vec![
                Point::new(0.0, 0.0),
                Point::new(5.0, -1.0),
                Point::new(3.0, -3.0),
            ];

            let mut rounder = PointsHash::new(6);
            for point in &points {
                rounder.hash_point(point);
            }
            let triangle = Polygon::new(points);
            let hexagons = hexagonal_tile(0.2, 0.2);
            let hexagons_segments = hexagons.tile(&triangle.get_quadrant(), &mut rounder);
            let clipping_segments: Vec<_> = triangle.segments().collect();
            let (inside, outside) = clip(&clipping_segments, &hexagons_segments, &mut rounder);
            let paths: Vec<_> = inside
                .into_iter()
                .map(|i| TaggedPath::Fill(ElementaryPath::Segment(i)))
                .chain(
                    outside
                        .into_iter()
                        .map(|o| TaggedPath::Shell(ElementaryPath::Segment(o))),
                )
                .collect();
            let mut g = Graph::new(&paths);
            g.even_degrees();
        });
    }
    #[bench]
    fn graph_linear_even_degrees(b: &mut Bencher) {
        b.iter(|| {
            let points = vec![
                Point::new(0.0, 0.0),
                Point::new(5.0, -1.0),
                Point::new(3.0, -3.0),
            ];

            let mut rounder = PointsHash::new(6);
            for point in &points {
                rounder.hash_point(point);
            }
            let triangle = Polygon::new(points);
            let hexagons = hexagonal_tile(0.2, 0.2);
            let hexagons_segments = hexagons.tile(&triangle.get_quadrant(), &mut rounder);
            let clipping_segments: Vec<_> = triangle.segments().collect();
            let (inside, outside) = clip(&clipping_segments, &hexagons_segments, &mut rounder);
            let paths: Vec<_> = inside
                .into_iter()
                .map(|i| TaggedPath::Fill(ElementaryPath::Segment(i)))
                .chain(
                    outside
                        .into_iter()
                        .map(|o| TaggedPath::Shell(ElementaryPath::Segment(o))),
                )
                .collect();
            let mut g = Graph::new(&paths);
            let nearby_vertices = g.nearby_vertices();
            g.fast_even_degrees(&nearby_vertices);
        });
    }

}
