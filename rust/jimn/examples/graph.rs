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

    let points2 = vec![
        Point::new(1.0, 1.0),
        Point::new(3.0, 1.0),
        Point::new(2.0, 5.0),
    ];

    let mut rounder = PointsHash::new(6);
    for point in points.iter().chain(points2.iter()) {
        rounder.hash_point(point);
    }
    let triangle1 = Polygon::new(points);
    let triangle2 = Polygon::new(points2);
    display!(triangle1, triangle2);

    let hexagons = hexagonal_tile(0.4, 0.4);

    let hexagons_segments = hexagons.tile(&triangle1.get_quadrant(), &mut rounder);
    let clipping_segments: Vec<_> = triangle1.segments().collect();
    let (inside1, outside1) = clip(&clipping_segments, &hexagons_segments, &mut rounder);

    let hexagons_segments = hexagons.tile(&triangle2.get_quadrant(), &mut rounder);
    let clipping_segments: Vec<_> = triangle2.segments().collect();
    let (inside2, outside2) = clip(&clipping_segments, &hexagons_segments, &mut rounder);

    let mut graph = Graph::new(
        inside1
            .iter()
            .chain(outside1.iter())
            .chain(inside2.iter())
            .chain(outside2.iter()),
    );
    display!(graph);
    let nearby_vertices = graph.nearby_vertices();
    graph.reconnect(&nearby_vertices);
    display!(graph);
}
