#[macro_use]
extern crate jimn;
use jimn::{Point, Polygon};
use jimn::utils::coordinates_hash::PointsHash;
use jimn::tycat::display;
use jimn::quadrant::{Quadrant, Shape};
use jimn::tile::hexagonal_tile;
use jimn::clipper::clip;
use jimn::graph::Graph;

fn main() {
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
    display!(triangle);
    let hexagons = hexagonal_tile(1.0, 1.0);
    let hexagons_segments = hexagons.tile(&triangle.get_quadrant(), &mut rounder);
    display!(triangle, hexagons_segments);
    let clipping_segments: Vec<_> = triangle.segments().collect();
    let (inside, outside) = clip(&clipping_segments, &hexagons_segments, &mut rounder);
    let mut graph = Graph::new(inside.iter().chain(outside.iter()));
    let vertices = graph.nearby_vertices();
    for group in vertices.iter().rev() {
        graph.vertices_tycat(group);
    }
    //    graph.even_degrees();
    //    let cycle = graph.eulerian_cycle();
    //    for i in 0..cycle.len() {
    //        let paths: Vec<_> = cycle.iter().take(i + 1).collect();
    //        display!(paths, paths.last().unwrap());
    //    }
}
