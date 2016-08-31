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
    points_neighbours: HashMap<Point, Vec<usize>>
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

    // Computes for each point the list of neighbouring paths.
    fn hash_points(&mut self) {
        for path in self.paths.values() {
            let points = (*path).points();
            if self.reversed_paths {
                // if paths are duplicated, only add start ;
                // end will be added by reversed path
                self.points_neighbours.entry(points[0])
                    .or_insert(Vec::new()).push(id(&path));
            } else {
                for endpoint in points {
                    self.points_neighbours.entry(*endpoint)
                        .or_insert(Vec::new()).push(id(&path));
                }
            }
        }
    }


    fn sort_neighbours_by_angle(&mut self) {
        let paths = &self.paths;
        let angle = |point: &Point, path_id: usize| {
            let path = paths.get(&path_id).expect("failed to find path");
            let other_point = path.endpoint_not(*point);
            point.angle_with(&other_point)
        };
        for (point, neighbours) in &mut self.points_neighbours {
            neighbours.sort_by(
                |p1, p2| {
                    let angle1 = angle(point, *p1);
                    let angle2 = angle(point, *p2);
                    angle1.partial_cmp(&angle2).unwrap()
                });
        }
    }
}

/// Turns given set of boxed segments into a set of polygons.
pub fn build_polygons(paths: Vec<Box<ElementaryPath>>) -> Vec<Polygon> {
    let mut builder = PocketsBuilder::new(paths, true);
    Vec::new()
}
