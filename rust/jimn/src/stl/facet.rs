//! Facet submodule for jimn.
//!
//! Provides `Facet` class for handling 3D facets from stl files.
use std::io::{Read, Seek, SeekFrom};
use byteorder::{ReadBytesExt, LittleEndian};
use ordered_float::NotNaN;

use quadrant::Quadrant;
use point::Point;
use segment::Segment;
use utils::coordinates_hash::{CoordinatesHash, PointsHash};
use stl::point3::Point3;

/// A `Facet` is just a triangle in space.
#[derive(Debug)]
pub struct Facet {
    points: [Point3; 3],
}

impl Facet {
    /// Parses binary content into of cursor on stl data into facet.
    pub fn new<R: Read + Seek>(raw_data: &mut R,
                               quadrant: &mut Quadrant,
                               heights: &mut CoordinatesHash)
                               -> Facet {
        #[inline]
        fn read_point<R: Read>(raw_data: &mut R,
                               quadrant: &mut Quadrant,
                               heights: &mut CoordinatesHash)
                               -> Point3 {
            let x = NotNaN::new(raw_data.read_f32::<LittleEndian>().unwrap() as f64).unwrap();
            let y = NotNaN::new(raw_data.read_f32::<LittleEndian>().unwrap() as f64).unwrap();
            let z = NotNaN::new(raw_data.read_f32::<LittleEndian>().unwrap() as f64).unwrap();
            let point = Point3::new(x, y, heights.hash_coordinate(z));
            quadrant.add(&point);
            point
        }
        //skip normal vector
        //no pb unwrapping since we already tested for size outside
        raw_data.seek(SeekFrom::Current(12)).unwrap();
        let new_facet = Facet {
            points: [read_point(raw_data, quadrant, heights),
                     read_point(raw_data, quadrant, heights),
                     read_point(raw_data, quadrant, heights)],
        };
        //skip useless bytes
        raw_data.seek(SeekFrom::Current(2)).unwrap();
        new_facet
    }

    /// Returns zmin and zmax.
    pub fn height_limits(&self) -> (NotNaN<f64>, NotNaN<f64>) {
        let mut z_coordinates: Vec<NotNaN<f64>> = self.points.iter().map(|p| p.z).collect();
        z_coordinates.sort();
        (z_coordinates[0], z_coordinates[2])
    }

    /// Intersects facet at given height.
    pub fn intersect(&self, height: NotNaN<f64>, hasher: &mut PointsHash) -> Option<Segment> {

        let mut intersections: Vec<Point> = [(0, 1), (0, 2), (1, 2)]
            .iter()
            .filter_map(|&(i, j)| {
                self.points[i].segment_intersection(&self.points[j], height, hasher)
            })
            .collect();

        intersections.sort();
        intersections.dedup();
        if intersections.len() == 2 {
            Some(Segment::new(intersections[0], intersections[1]))
        } else {
            None
        }
    }
}
