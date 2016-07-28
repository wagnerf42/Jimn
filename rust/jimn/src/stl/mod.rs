//! Handles STL files.
//!
//! Provides **Stl** class handling 3d models from stl files.
//! Color information is discarded.
use byteorder::{ReadBytesExt, LittleEndian};

mod facet;
mod point3;
use stl::facet::Facet;
use std::fs::File;
use std::io::Error;
use std::io::Read;
use std::io::SeekFrom;
use std::io::Seek;
use std::io::Cursor;

/// The **Stl** structure holds a set of [facets](facet/struct.Facet.html).
pub struct Stl {
    /// Vector containing all facets.
    pub facets: Vec<Facet>
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
        let mut model = Stl { facets: Vec::with_capacity(size) };
        let mut facets_data = Cursor::new(buffer);
        for _ in 0..facets_number {
            model.facets.push(Facet::new(&mut facets_data));
        }
        Ok(model)
    }
}
