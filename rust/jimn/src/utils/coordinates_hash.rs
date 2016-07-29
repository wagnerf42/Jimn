//! Fast identification of nearby points.
//!
//! Provides a `CoordinatesHash` structure which is used to hash
//! nearby points together in O(1).

use std::collections::HashMap;
use point::Point;

/// a `CoordinatesHash` allows for hashing nearby points together in O(1).
pub struct CoordinatesHash {
    hashes: Vec<HashMap<String, Point>>
}

//TODO: how to change precision in the format ?
fn coordinate_key(coordinate: f64) -> String {
    format!("{:.6}", coordinate)
}

fn displaced_coordinate_key(coordinate: f64) -> String {
    coordinate_key(10.0f64.powi(-6)+ coordinate)
}

impl CoordinatesHash {
    /// creates a new `CoordinatesHash` with given space dimension.
    pub fn new(dimension: u32) -> CoordinatesHash {
        CoordinatesHash {
            hashes: vec![HashMap::new(); 2<<dimension]
        }
    }

    fn compute_key(&self, hash_number: usize, point: &Point) -> String {
        let mut key_parts: Vec<String> = Vec::new();
        let mut remaining_bits = hash_number;
        for coordinate in point.coordinates() {
            if (remaining_bits % 2) == 1 {
                key_parts.push(displaced_coordinate_key(coordinate));
            } else {
                key_parts.push(coordinate_key(coordinate));
            }
            remaining_bits /= 2;
        }
        key_parts.join(";")
    }

    //TODO: add fast hash ?
    /// Tries to add a point to the hash.
    /// If a nearby point was already there
    /// returns the nearby point, else adds point and returns it.
    pub fn hash_point(&mut self, point: &Point) -> Point {
        let mut keys: Vec<String> = Vec::new();
        for (index, hash) in self.hashes.iter().enumerate() {
            let key = self.compute_key(index, point);
            let possible_old_point = hash.get(&key);
            if possible_old_point.is_some() {
                return *possible_old_point.unwrap();
            } else {
                keys.push(key);
            }
        }
        for (key, hash) in keys.into_iter().zip(self.hashes.iter_mut()) {
            hash.insert(key, *point);
        }
        *point
    }
}
