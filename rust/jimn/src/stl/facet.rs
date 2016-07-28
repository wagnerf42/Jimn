//! Facet submodule for jimn.
//!
//! Provides **Facet** class for handling 3D facets from stl files.
use byteorder::{ReadBytesExt, LittleEndian};
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

    fn find_points_above_and_below(&self, height: f64)
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

    //Intersects facet at given height.
    pub fn intersect(&self, height: f64) -> Option<Segment> {
        println!("{:?}", self);
        let (lower_points, higher_points) = self.find_points_above_and_below(height);
        //TODO: filter remaining facets
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
        println!("iso:{:?}", isolated_point);
        println!("TODO");
        None
    }
}
