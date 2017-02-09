//! Handles STL files.
//!
//! Provides **Stl** class handling 3d models from stl files.
//! Color information is discarded.
use std::fs::File;
use std::io::{Error, Read, SeekFrom, Seek, Cursor};
use byteorder::{ReadBytesExt, LittleEndian};

mod facet;
mod point3;
//use segment::Segment;
use stl::facet::Facet;
use quadrant::Quadrant;
use utils::coordinates_hash::{CoordinatesHash, PointsHash};

/// The **Stl** structure holds a set of [facets](facet/struct.Facet.html).
pub struct Stl {
    /// Vector containing all facets.
    pub facets: Vec<Facet>,
    /// Box containing all 3D points.
    pub dimensions: Quadrant,
    /// hash of heights (needed to align nearby heights coordinate)
    heights: CoordinatesHash
}

impl Stl {
    /// Loads a new stl model from given file.
    pub fn new(filename: &str) -> Result<Stl, Error> {
        let mut file = try!(File::open(filename));
        //read header
        try!(file.seek(SeekFrom::Start(80)));
        let facets_number = try!(file.read_u32::<LittleEndian>());

        // for each facet 4 vectors of 3 32bits floats + 2 unused bytes
        let size = (facets_number * (4*3*4+2)) as usize;

        let mut buffer:Vec<u8> = Vec::with_capacity(size);
        let loaded = try!(file.take(size as u64).read_to_end(&mut buffer));
        //TODO: replace assert with error
        assert_eq!(loaded, size);

        // parse facets
        let mut model = Stl {
            facets: Vec::with_capacity(size),
            dimensions: Quadrant::new(3),
            heights: CoordinatesHash::new(5)
        };
        let mut facets_data = Cursor::new(buffer);
        for _ in 0..facets_number {
            model.facets.push(
                Facet::new(&mut facets_data,
                           &mut model.dimensions,
                           &mut model.heights));
        }
        Ok(model)
    }

    /// Cuts model into slices of given thickness (starting at the top).
    /// Returns vector of tuples (height, slice).
    pub fn compute_slices(&mut self, thickness: f64,
                          hasher: &mut PointsHash) {
                          //hasher: &mut PointsHash) -> Vec<(f64, Vec<Segment>)> {
        let (min_height, max_height) = self.dimensions.limits(2);
        let slices_number = ((max_height - min_height)/thickness).ceil() as usize;
        panic!("TODO");
    }
}
