//! Facet submodule for jimn.
//!
//! Provides **Facet** class for handling 3D facets from stl files.
use byteorder::{ReadBytesExt, LittleEndian};
use stl::point3::Point3;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;

/// A facet is just a triangle in space.
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
}
