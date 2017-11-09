#[macro_use]
extern crate jimn;
use jimn::{ElementaryPath, HoledPolygon, Point, Polygon, TaggedPath};
use jimn::quadrant::Shape;
use jimn::utils::coordinates_hash::PointsHash;
use jimn::tile::hexagonal_tile;
use jimn::clipper::clip;
use jimn::graph::Graph;
use jimn::offsetter::offset_holed_polygon;


fn main() {
    let mut rounder = PointsHash::new(6);
    let points = vec![
        vec![
            rounder.hash_point(&Point::new(3.0, -3.0)),
            rounder.hash_point(&Point::new(4.0, -1.5)),
            rounder.hash_point(&Point::new(4.5, -2.0)),
            rounder.hash_point(&Point::new(5.0, -1.0)),
            rounder.hash_point(&Point::new(0.0, 0.0)),
        ],
        vec![
            rounder.hash_point(&Point::new(1.0, 1.0)),
            rounder.hash_point(&Point::new(3.0, 1.0)),
            rounder.hash_point(&Point::new(5.0, 5.0)),
        ],
        vec![
            rounder.hash_point(&Point::new(-1.0, 3.0)),
            rounder.hash_point(&Point::new(2.0, 3.0)),
            rounder.hash_point(&Point::new(1.0, 8.0)),
        ],
        vec![
            rounder.hash_point(&Point::new(5.0, 2.0)),
            rounder.hash_point(&Point::new(5.0, 1.0)),
            rounder.hash_point(&Point::new(7.0, 2.0)),
        ],
// This creates a rounding bug in angles in BO
//        vec![
//            rounder.hash_point(&Point::new(5.0, 2.0)),
//            rounder.hash_point(&Point::new(5.0, 1.0)),
//            //rounder.hash_point(&Point::new(7.0, 2.0)),
//            rounder.hash_point(&Point::new(7.0, 3.0)),
//        ],
    ];

    let polygons: Vec<_> = points
        .into_iter()
        .map(|p| HoledPolygon::new(Polygon::new(p), Vec::new()))
        .collect();
    display!(multicolor!(&polygons));

    let pockets: Vec<_> = polygons
        .iter()
        .flat_map(|t| offset_holed_polygon(t, 0.2, &mut rounder))
        .collect();
    display!(multicolor!(&pockets));

    let hexagons = hexagonal_tile(0.8, 0.8);
    let mut paths: Vec<TaggedPath> = Vec::new();
    for pocket in &pockets {
        let segments: Vec<ElementaryPath> = hexagons
            .tile(&pocket.get_quadrant(), &mut rounder)
            .into_iter()
            .map(ElementaryPath::Segment)
            .collect();
        let (inside, outside) = clip(pocket.paths(), &segments, &mut rounder);
        paths.extend(outside.into_iter().map(TaggedPath::Shell));
        paths.extend(inside.into_iter().map(TaggedPath::Fill));
    }
    display!(unicolor!(&paths));

    let mut graph = Graph::new(&paths);
    display!(graph);
    let nearby_vertices = graph.nearby_vertices();
    graph.reconnect(&nearby_vertices);
    display!(graph);
    graph.fast_even_degrees(&nearby_vertices);
    display!(graph);
    let cycle = graph.eulerian_cycle();
    let mut partial_path = Vec::new();
    for path in &cycle {
        partial_path.push(path);
        display!(unicolor!(&partial_path));
    }
}
