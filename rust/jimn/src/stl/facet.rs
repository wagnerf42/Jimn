//! Facet submodule for jimn.
//!
//! Provides `Facet` class for handling 3D facets from stl files.
use std::io::{Read, Seek, SeekFrom};
use byteorder::{ReadBytesExt, LittleEndian};
use ordered_float::NotNaN;

use quadrant::Quadrant;
use point::Point;
//use segment::Segment;
use utils::precision::is_almost;
use utils::coordinates_hash::{CoordinatesHash, PointsHash};
use stl::point3::Point3;

/// A `Facet` is just a triangle in space.
#[derive(Debug)]
pub struct Facet {
    points: [Point3; 3]
}

impl Facet {
    /// Parses binary content into of cursor on stl data into facet.
    pub fn new<R: Read + Seek>(raw_data: &mut R,
                               quadrant: &mut Quadrant,
                               heights: &mut CoordinatesHash) -> Facet {
        #[inline]
        fn read_point<R: Read>(raw_data: &mut R,
                               quadrant: &mut Quadrant,
                               heights: &mut CoordinatesHash) -> Point3 {
            let point = Point3::new(
                NotNaN::new(raw_data.read_f32::<LittleEndian>().unwrap() as f64).unwrap(),
                NotNaN::new(raw_data.read_f32::<LittleEndian>().unwrap() as f64).unwrap(),
                heights.hash_coordinate(
                    NotNaN::new(raw_data.read_f32::<LittleEndian>().unwrap() as f64).unwrap()));
            quadrant.add(&point);
            point
        }
        //skip normal vector
        //no pb unwrapping since we already tested for size outside
        raw_data.seek(SeekFrom::Current(12)).unwrap();
        let new_facet = Facet {
            points: [
                read_point(raw_data, quadrant, heights),
                read_point(raw_data, quadrant, heights),
                read_point(raw_data, quadrant, heights)
            ]
        };
        //skip useless bytes
        raw_data.seek(SeekFrom::Current(2)).unwrap();
        new_facet
    }

    /// Intersects facet at given height.
    pub fn intersect(&self, height: f64,
                     hasher: &mut PointsHash) {
                     //hasher: &mut PointsHash) -> Option<Segment> {
        panic!("TODO")
     }
}
