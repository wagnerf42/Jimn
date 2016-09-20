//! Build polygons out of segments.
use std::collections::Bound::{Excluded, Unbounded};
use std::collections::{HashMap, BTreeMap};
use std::cmp::{Ord, Ordering};
use point::Point;
use segment::Segment;
use polygon::Polygon;
use elementary_path::ElementaryPath;


#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
struct AngleKey {
    angle: f64,
}

struct PolygonsBuilder {
    points: HashMap<Point, BTreeMap<AngleKey, Segment>>
}

impl AngleKey {
    fn new(segment: &Segment, endpoint: &Point) -> AngleKey {
        let other_point = segment.endpoint_not(endpoint);
        AngleKey {angle: endpoint.angle_with(&other_point)}
    }
}

impl Eq for AngleKey {}
impl Ord for AngleKey {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.angle == other.angle {
            Ordering::Equal
        } else {
            match self.angle < other.angle {
                true => Ordering::Less,
                false => Ordering::Greater
            }
        }
    }
}

impl PolygonsBuilder {
    fn new(segments: Vec<Segment>) -> PolygonsBuilder {
        let mut builder = PolygonsBuilder {
            points: HashMap::new()
        };

        for segment in segments.into_iter()
            .flat_map(
                |s| vec![Segment::new(s.end(), s.start()), s].into_iter()) {

            let start = segment.start();
            builder.points.entry(start)
                .or_insert(BTreeMap::new())
                .insert(AngleKey::new(&segment, &start), segment);
        }
        builder
    }

    fn finalize(&mut self) -> Vec<Polygon> {
        let mut polygons = Vec::new();
        while !self.points.is_empty() {
            polygons.push(self.build_polygon());
        }
        polygons
    }

    fn remove_segment(&mut self, point: Point, key: AngleKey) -> Segment {
        //TODO: change when BTreeMap will not be such a mess
        let neighbour;
        let delete_point;
        {
            let neighbours = self.points.get_mut(&point).unwrap();
            neighbour = neighbours.remove(&key).unwrap();
            delete_point = neighbours.is_empty();
        }
        if delete_point {
            self.points.remove(&point);
        }
        neighbour
    }
    
    fn find_start(&self) -> (Point, AngleKey) {
        let start_point = *self.points.keys().next().unwrap();
        let start_segments = self.points.get(&start_point).unwrap();
        let start_key = *start_segments.keys().next().unwrap();
        (start_point, start_key)
    }

    fn get_start_segment(&mut self) -> Segment {
        let (point, key) = self.find_start();
        self.remove_segment(point, key)
    }

    fn next_segment(&mut self, from: &Segment) -> Segment {
        // we look for first segment with higher key
        let next_point = from.end();
        let key;
        {
            let next_segments = self.points.get(&next_point).unwrap();
            key = match next_segments
                .range(Excluded(&AngleKey::new(&from, &next_point)), Unbounded).next() {
                    Some(k) => *k.0,
                    None => *next_segments.keys().next().unwrap()
                };
        }
        self.remove_segment(next_point, key)
    }

    fn build_polygon(&mut self) -> Polygon {
        let mut points = Vec::new();
        let mut current_segment = self.get_start_segment();
        let (start_point, mut current_point) = current_segment.points();
        points.push(current_point);
        while start_point != current_point {
            current_segment = self.next_segment(&current_segment);
            current_point = current_segment.end();
            points.push(current_point);
        }
        Polygon {points: points}
    }
}

/// Turns given set of segments into a set of polygons.
pub fn build_polygons(segments: Vec<Segment>) -> Vec<Polygon> {
    let raw_polygons = PolygonsBuilder::new(segments).finalize();
    raw_polygons.iter().filter(|p| p.is_oriented_clockwise())
        .map(|p| p.simplify()).collect()
}
