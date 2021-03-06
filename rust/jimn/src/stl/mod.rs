//! Handles STL files.
//!
//! Provides **Stl** class handling 3d models from stl files.
//! Color information is discarded.
use std::fs::File;
use std::io::{Cursor, Error, Read, Seek, SeekFrom};
use std::collections::HashSet;
use byteorder::{LittleEndian, ReadBytesExt};

mod facet;
mod point3;
use segment::Segment;
use stl::facet::Facet;
use quadrant::Quadrant;
use utils::coordinates_hash::{CoordinatesHash, PointsHash};

/// The **Stl** structure holds a set of [Facets](facet/struct.Facet.html).
pub struct Stl {
    /// Vector containing all facets.
    pub facets: Vec<Facet>,
    /// `Quadrant` containing all 3D points.
    pub dimensions: Quadrant,
    /// hash of heights (needed to align nearby heights coordinate)
    heights: CoordinatesHash,
}

/// slicing algorithm is event based
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
enum EventType {
    FacetEnd,
    Cut,
    FacetStart,
}

struct CuttingEvent {
    /// Height at which event is happening
    height: f64,
    /// Event type
    event_type: EventType,
    /// Optional facet for facets creations / destructions
    facet: Option<usize>,
}

impl Stl {
    /// Loads a new stl model from given file.
    pub fn new(filename: &str) -> Result<Stl, Error> {
        let mut file = File::open(filename)?;
        //read header
        file.seek(SeekFrom::Start(80))?;
        let facets_number = file.read_u32::<LittleEndian>()?;

        // for each facet 4 vectors of 3 32bits floats + 2 unused bytes
        let size = (facets_number * (4 * 3 * 4 + 2)) as usize;

        let mut buffer: Vec<u8> = Vec::with_capacity(size);
        let loaded = file.take(size as u64).read_to_end(&mut buffer)?;
        //TODO: replace assert with error
        assert_eq!(loaded, size);

        // parse facets
        let mut model = Stl {
            facets: Vec::with_capacity(size),
            dimensions: Quadrant::new(3),
            heights: CoordinatesHash::new(5),
        };
        let mut facets_data = Cursor::new(buffer);
        for _ in 0..facets_number {
            model.facets.push(Facet::new(
                &mut facets_data,
                &mut model.dimensions,
                &mut model.heights,
            ));
        }
        Ok(model)
    }

    /// Prepares for cutting by generating all events.
    fn generate_cutting_events(&self, thickness: f64) -> Vec<CuttingEvent> {
        let (min_height, max_height) = self.dimensions.limits(2);
        let height = max_height - min_height;
        let slices_number = (height / thickness).ceil() as usize;
        let extra_height = (thickness * (slices_number as f64) - height) / 2.0;
        let cut_heights = (0..slices_number)
            .map(|z| min_height - extra_height + thickness / 2.0 + thickness * (z as f64));
        let mut events: Vec<CuttingEvent> =
            Vec::with_capacity(slices_number + 2 * self.facets.len());
        for (index, facet) in self.facets.iter().enumerate() {
            let (zmin, zmax) = facet.height_limits();
            let start_event = CuttingEvent {
                height: zmin,
                event_type: EventType::FacetStart,
                facet: Some(index),
            };
            events.push(start_event);
            let end_event = CuttingEvent {
                height: zmax,
                event_type: EventType::FacetEnd,
                facet: Some(index),
            };
            events.push(end_event);
        }

        for height in cut_heights {
            events.push(CuttingEvent {
                height: self.heights.lookup_coordinate(height),
                event_type: EventType::Cut,
                facet: None,
            });
        }
        events
    }

    /// Cuts model into slices of given thickness.
    /// Returns vector of tuples (height, slice).
    pub fn compute_slices(
        &self,
        thickness: f64,
        hasher: &mut PointsHash,
    ) -> Vec<(f64, Vec<Segment>)> {
        let mut events = self.generate_cutting_events(thickness);
        events.sort_by(|a, b| {
            a.height
                .partial_cmp(&b.height)
                .unwrap()
                .then(a.event_type.cmp(&b.event_type))
        });
        let mut facets: HashSet<usize> = HashSet::new();
        let mut slices = Vec::new();
        for event in &events {
            match event.event_type {
                EventType::Cut => {
                    let intersections: Vec<Segment> = facets
                        .iter()
                        .filter_map(|i: &usize| self.facets[*i].intersect(event.height, hasher))
                        .collect();
                    slices.push((event.height, intersections));
                }
                EventType::FacetStart => {
                    facets.insert(event.facet.unwrap());
                }
                EventType::FacetEnd => {
                    facets.remove(&event.facet.unwrap());
                }
            }
        }
        slices
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    #[bench]
    fn slicer_small_object(b: &mut Bencher) {
        let stl = Stl::new("../../test_files/cordoba.stl").expect("failed loading stl file");
        b.iter(|| {
            let mut hasher = PointsHash::new(5);
            stl.compute_slices(0.1, &mut hasher);
        })
    }
    #[bench]
    fn slicer_large_object(b: &mut Bencher) {
        let stl = Stl::new("../../test_files/cordoba-large.stl").expect("failed loading stl file");
        b.iter(|| {
            let mut hasher = PointsHash::new(5);
            stl.compute_slices(0.1, &mut hasher);
        })
    }
}
