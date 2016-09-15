//! Build pockets out of paths.
use std::collections::HashMap;
use elementary_path::ElementaryPath;
use pocket::Pocket;
use polygon::Polygon;
use point::Point;

struct PocketsBuilder {
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
        builder.hash_points();
        builder.sort_neighbours_by_angle();
        builder
    }

    // Computes for each point the list of neighbouring paths.
    fn hash_points(&mut self) {
        for path in self.paths.values() {
            let (start, end) = path.points();
            self.points_neighbours.entry(start)
                .or_insert(Vec::new()).push(id(&path));
            self.points_neighbours.entry(end)
                .or_insert(Vec::new()).push(id(&path));
        }
    }

    fn sort_neighbours_by_angle(&mut self) {
        let paths = &self.paths;
        let angle = |point: &Point, path_id: usize| {
            let path = paths.get(&path_id).expect("failed to find path");
            let other_point = path.endpoint_not(point);
            let angle = point.angle_with(&other_point);
            //starting paths go first
            match path.start() == *point {
                true => (angle, 0),
                false => (angle, 1),
            }
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
            pockets.push(self.build_pocket(start_id));
        }
        pockets
    }

    fn build_pocket(&mut self, start_id: usize) -> Pocket {
        // start with start_path and follow edge building the pocket
        let mut current_id = start_id;
        let (start_point, mut current_point) = self.paths.get(&start_id)
            .unwrap().points();

        let mut edge = Vec::new();
        let mut previous_point = start_point;

        while current_point != start_point {
            let (next_path_id, next_point) = self.next_path(&current_point,
                                                            &previous_point);
            previous_point = current_point;
            current_point = next_point;

            //now, remove current path from hash and insert it in edge
            edge.push(self.paths.remove(&current_id)
                      .expect("cannot remove current path"));
            current_id = next_path_id;
        }
        //add last elementary path
        edge.push(self.paths.remove(&current_id)
                  .expect("cannot remove last path"));
        Pocket::new(edge)
    }

    fn next_path(&mut self, current_point: &Point, previous_point: &Point)
        -> (usize, Point) {
            // we came from previous point and are now at current point.
            // return what is next point.
            let neighbours = self.points_neighbours
                .get(current_point).expect("no neighbours for current point");

            let mut valid_paths = neighbours.iter()
                .cycle()
                .filter_map(|path_id: &usize| self.paths.get(path_id));
            
            //remove paths until finding incoming path
            valid_paths.find(|&path| path.start() == *previous_point)
                .expect("cannot find current path in remaining paths");
            // now find outgoing path 
            // it is tricky because paths are oriented
            // if you have : current_path, in_path, out_path, out_path2
            // in_path exits through out_path (directly nearby)
            // current_path exits through out_path2
            // we need to loop starting at current path
            // counting the # of incoming_paths met - # of outgoing ones
            // first outgoing with positive counter is kept
            let mut incoming_count = 0;
            for path in valid_paths {
                let path_start = path.start();
                if path_start == *current_point {
                    if incoming_count == 0 {
                        let next_point = path.end();
                        return (id(path), next_point);
                    }
                    incoming_count -= 1;
                } else {
                    incoming_count += 1;
                }
            }
            panic!("no way to find next path")
        }
}

/// Turns given set of boxed segments into a set of polygons.
pub fn build_polygons(paths: Vec<Box<ElementaryPath>>) -> Vec<Polygon> {
    let mut builder = PocketsBuilder::new(paths, true);
    let pockets:Vec<Pocket> = builder.build_pockets();
    let polygons = pockets.iter().map(|pocket| pocket.to_polygon())
        .filter(|p| p.is_oriented_clockwise()).collect();
    polygons
}
