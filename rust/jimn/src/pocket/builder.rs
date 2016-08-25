//! Build pockets out of paths.
use std::collections::HashMap;
use elementary_path::ElementaryPath;
use pocket::Pocket;
use polygon::Polygon;
use tycat::{Displayable, display};
use point::Point;

struct PocketsBuilder {
    pockets: Vec<Pocket>,
    reversed_paths: bool,
    paths: HashMap<usize, Box<ElementaryPath>>,
    points_neighbours: HashMap<Point, Vec<Box<ElementaryPath>>>
}

fn id(boxed: &Box<ElementaryPath>) -> usize {
    //cast in fat pointer then thin pointer then address
    &(**boxed) as *const _ as *const () as usize
}

impl PocketsBuilder {
    fn new(paths: Vec<Box<ElementaryPath>>,
           reversed_paths: bool) -> PocketsBuilder {
        let mut builder = PocketsBuilder {
            pockets: Vec::new(),
            reversed_paths: reversed_paths,
            paths: HashMap::new(),
            points_neighbours: HashMap::new()
        };

        for path in paths {
            if reversed_paths {
                let reversed = path.reverse();
                builder.paths.insert(id(&reversed), reversed);
            }
            builder.paths.insert(id(&path), path);
        }
        {
        display!(builder.paths);
        }
        builder.hash_points();
        builder.sort_neighbours_by_angle();
        builder
    }

    // Computes for each point the list of neighbouring points.
    fn hash_points(&mut self) {
        ()
    }

    fn sort_neighbours_by_angle(&mut self) {
        ()
    }
}

/// Turns given set of boxed segments into a set of polygons.
pub fn build_polygons(paths: Vec<Box<ElementaryPath>>) -> Vec<Polygon> {
    let mut builder = PocketsBuilder::new(paths, true);
    Vec::new()
}
