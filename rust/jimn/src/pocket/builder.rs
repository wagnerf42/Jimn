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
        display!(builder.paths);
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

    fn build_pockets(&mut self) -> Vec<Pocket> {
        let mut pockets = Vec::new();
        while !self.paths.is_empty() {
            let start_id = *self.paths.keys().next().unwrap();
            let start_path = self.paths.remove(&start_id).unwrap();
            pockets.push(self.build_pocket(start_path))
        }
        pockets
    }

    fn build_pocket(&mut self, start_path: Box<ElementaryPath>) -> Pocket {
        // start with start_path and follow edge building the pocket
        let mut current_path = Vec::new();
        let mut start_point;
        let mut current_point;
        {
            let points = start_path.points(); // TODO: figure out slice pattern ?
            start_point = points[0];
            current_point = points[1];
        }
        let mut previous_point = start_point;
        current_path.push(start_path);

//        while current_point != start_point {
//            let next_path = self.find_next_path(&current_point, &previous_point);
//            current_path.push(next_path);
//            self.paths.remove(&id(&next_path));
//            // continue moving
//            previous_point = current_point;
//            current_point = next_path.end();
//        }
        Pocket::new(current_path)
    }

//    fn find_next_path(&self, current_point: &Point, previous_point: &Point)
//        -> Box<ElementaryPath> {
//            // we came from previous point and are not at current point.
//            // return what is next point.
//            let neighbours = self.points_neighbours.get(current_point).unwrap();
//            let paths = neighbours.iter().map(
//                |path_id: &usize| self.paths.get(path_id).unwrap());
//
//            let path_index = paths.enumerate()
//                .find(|&(index, path)| path.end() == *current_point && path.start() == *previous_point)
//                .unwrap().0;
//            // now find next index
//            // it is tricky because paths are oriented
//            // if you have : current_path, in_path, out_path, out_path2
//            // in_path exits through out_path (directly nearby)
//            // current_path exits through out_path2
//            // we need to loop starting at current path
//            // counting the # of incoming_paths met - # of outgoing ones
//            // first outgoing with positive counter is kept
//            let mut incoming_count = 0;
//            let length = neighbours.len();
//            for neighbour in (1..length-1).map(|i| neighbours[(path_index + i)%length]) {
//                let path = self.paths.get(&neighbour).unwrap();
//                if path.start() == *current_point {
//                    if incoming_count == 0 {
//                        return *path;
//                    }
//                    incoming_count -= 1;
//                } else {
//                    incoming_count += 1;
//                }
//            }
//            panic!("no way to find next path")
//    }
}

/// Turns given set of boxed segments into a set of polygons.
pub fn build_polygons(paths: Vec<Box<ElementaryPath>>) -> Vec<Polygon> {
    let mut builder = PocketsBuilder::new(paths, true);
    let pockets = builder.build_pockets();
    display!(pockets);
    Vec::new()
}
