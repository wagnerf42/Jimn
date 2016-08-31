//! Handles STL files.
//!
//! Provides **Stl** class handling 3d models from stl files.
//! Color information is discarded.
use std::fs::File;
use std::io::{Error, Read, SeekFrom, Seek, Cursor};
use byteorder::{ReadBytesExt, LittleEndian};

mod facet;
mod point3;
use segment::Segment;
use stl::facet::Facet;
use bounding_box::BoundingBox;

/// The **Stl** structure holds a set of [facets](facet/struct.Facet.html).
pub struct Stl {
    /// Vector containing all facets.
    pub facets: Vec<Facet>,
    /// Box containing all 3D points.
    pub dimensions: BoundingBox
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
            dimensions: BoundingBox::empty_box(3)
        };
        let mut facets_data = Cursor::new(buffer);
        for _ in 0..facets_number {
            model.facets.push(
                Facet::new(&mut facets_data, &mut model.dimensions));
        }
        Ok(model)
    }

    /// Cuts model into slices of given thickness (starting at the top).
    /// Returns vector of tuples (height, slice).
    pub fn compute_slices(&self, thickness: f64) -> Vec<(f64, Vec<Segment>)> {
        let (min_height, max_height) = self.dimensions.limits(2);
        let slices_number = ((max_height - min_height)/thickness).ceil() as usize;
        let mut remaining_facets:Vec<&Facet> = self.facets.iter().collect();
        let mut slices = Vec::with_capacity(slices_number);

        for slice_number in 0..slices_number {
            let mut lower_boundary = max_height - ((slice_number + 1) as f64) * thickness;
            //TODO: hash heights
            //lower_boundary = self.heights_hash.hash_coordinate(lower_boundary);
            if lower_boundary < min_height + 0.01 {
                lower_boundary = min_height + 0.01; //TODO: do a special case instead
            }
            //discard all facets too high
            remaining_facets = remaining_facets.into_iter()
                .filter(|f| f.is_below(lower_boundary)).collect();
            //cut facets at given height
            let current_slice = remaining_facets.iter()
                .filter_map(|f| f.intersect(lower_boundary)).collect();
            slices.push((lower_boundary, current_slice));
        }
        slices
    }
}
