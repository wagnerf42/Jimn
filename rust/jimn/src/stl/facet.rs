//! Facet submodule for jimn.
//!
//! Provides **Facet** class for handling 3D facets from stl files.
use byteorder::{ReadBytesExt, LittleEndian};
use point::Point;
use segment::Segment;
use utils::precision::is_almost;
use stl::point3::Point3;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;

/// A facet is just a triangle in space.
#[derive(Debug)]
pub struct Facet {
    points: [Point3; 3]
}

impl Facet {
    /// Parses binary content into of cursor on stl data into facet.
    pub fn new<R: Read + Seek>(raw_data: &mut R) -> Facet {
        #[inline]
        fn read_point<R: Read>(raw_data: &mut R) -> Point3 {
            Point3::new(
                raw_data.read_f32::<LittleEndian>().unwrap() as f64,
                raw_data.read_f32::<LittleEndian>().unwrap() as f64,
                raw_data.read_f32::<LittleEndian>().unwrap() as f64)
        }
        //skip normal vector
        //no pb unwrapping since we already tested for size outside
        raw_data.seek(SeekFrom::Current(12)).unwrap();
        let new_facet = Facet {
            points: [
                read_point(raw_data),
                read_point(raw_data),
                read_point(raw_data)
            ]
        };
        //skip useless bytes
        raw_data.seek(SeekFrom::Current(2)).unwrap();
        new_facet
    }

    /// Returns two vector of references on our points.
    /// First one contains points below given height and other
    /// one points above given height.
    fn points_above_below(&self, height: f64)
        -> (Vec<&Point3>, Vec<&Point3>) {
            let (mut low_points, mut high_points) = (Vec::new(), Vec::new());
            for point in &self.points {
                if point.z > height {
                    high_points.push(point)
                } else {
                    low_points.push(point)
                }
            }
            (low_points, high_points)
    }

    /// Intersects facet at given height.
    //TODO: filter remaining facets
    pub fn intersect(&self, height: f64) -> Option<Segment> {
        let (lower_points, higher_points) = self.points_above_below(height);
        let (together_points, isolated_point);
        if lower_points.len() == 2 {
            together_points = lower_points;
            isolated_point = higher_points[0];
        } else if higher_points.len() == 2 {
            together_points = higher_points;
            isolated_point = lower_points[0];
            if is_almost(isolated_point.z, height) {
                return None
            }
        } else {
            return None
        }
        // intersect segments crossing height
        let intersection_points: Vec<Point> = together_points.iter()
            .map(|p| p.segment_intersection(&isolated_point, height)).collect();
        // because of rounding
        if intersection_points[0].is_almost(&intersection_points[1]) {
            return None
        }
        Some(Segment::new(intersection_points[0], intersection_points[1]))
    }
}
