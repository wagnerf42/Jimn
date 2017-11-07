#[macro_use]
extern crate jimn;
use jimn::{HoledPolygon, Point, Polygon, Segment};
use jimn::utils::coordinates_hash::PointsHash;
use jimn::tycat::colored_display;
use jimn::tile::hexagonal_tile;
use jimn::clipper::clip;
use jimn::graph::Graph;
use jimn::offsetter::offset_holed_polygon;

fn main() {
    let mut rounder = PointsHash::new(6);
    let points = vec![
        vec![
            rounder.hash_point(&Point::new(3.0, -3.0)),
            rounder.hash_point(&Point::new(5.0, -1.0)),
            rounder.hash_point(&Point::new(0.0, 0.0)),
        ],
//        vec![
//            rounder.hash_point(&Point::new(1.0, 1.0)),
//            rounder.hash_point(&Point::new(3.0, 1.0)),
//            rounder.hash_point(&Point::new(5.0, 5.0)),
//        ],
//        vec![
//            rounder.hash_point(&Point::new(-1.0, 3.0)),
//            rounder.hash_point(&Point::new(2.0, 3.0)),
//            rounder.hash_point(&Point::new(1.0, 8.0)),
//        ],
//        vec![
//            rounder.hash_point(&Point::new(5.0, 2.0)),
//            rounder.hash_point(&Point::new(5.0, 1.0)),
//            rounder.hash_point(&Point::new(7.0, 2.0)),
//        ],
    ];

    let triangles: Vec<_> = points
        .into_iter()
        .map(|p| HoledPolygon::new(Polygon::new(p), Vec::new()))
        .collect();
    colored_display(triangles.iter()).expect("display failed");

    let pockets: Vec<_> = triangles
        .iter()
        .map(|t| offset_holed_polygon(t, 0.2, &mut rounder))
        .collect();
    colored_display(pockets.iter()).expect("display failed");

    let hexagons = hexagonal_tile(0.8, 0.8);
    //    let mut segments: Vec<Segment> = Vec::new();
    //
    //    for triangle in &triangles {
    //        let hexagons_segments = hexagons.tile(&triangle.get_quadrant(), &mut rounder);
    //        let clipping_segments: Vec<_> = triangle.segments().collect();
    //        let (inside, outside) = clip(&clipping_segments, &hexagons_segments, &mut rounder);
    //        segments.extend(inside);
    //        segments.extend(outside);
    //    }
    //
    //    let mut graph = Graph::new(&segments);
    //    display!(graph);
    //    let nearby_vertices = graph.nearby_vertices();
    //    graph.reconnect(&nearby_vertices);
    //    display!(graph);
    //    graph.fast_even_degrees(&nearby_vertices);
    //    display!(graph);
}
