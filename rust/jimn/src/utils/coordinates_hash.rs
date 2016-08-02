//! Fast identification of nearby points.
//!
//! Provides a `CoordinatesHash` structure which is used to hash
//! nearby points together in O(1).
//! The `CoordinatesHash` is created with a given space dimension and
//! a given precision.
//! Considering a distance given by infinity norm of difference vector,
//! we have the following guarantees:
//! 
//! * any two points with distance < 5 * 10^-(precision+1) are hashed together.
//! * no points of distance > 10^-precision are hashed together.

use std::collections::HashMap;
use point::Point;

/// a `CoordinatesHash` allows for hashing nearby points together in O(1).
pub struct CoordinatesHash {
    hashes: Vec<HashMap<String, Point>>,
    precision: usize
}

fn coordinate_key(coordinate: f64, precision: usize) -> String {
    format!("{:.p$}", coordinate, p=precision)
}

fn displaced_coordinate_key(coordinate: f64, precision: usize) -> String {
    coordinate_key(5.0 * 10.0f64.powi(-((precision+1) as i32))+ coordinate, precision)
}

impl CoordinatesHash {
    /// Creates a new `CoordinatesHash` with given space dimension.
    /// and given precision.
    pub fn new(dimension: u32, precision: usize) -> CoordinatesHash {
        CoordinatesHash {
            hashes: vec![HashMap::new(); 1<<dimension],
            precision: precision
        }
    }

    fn compute_key(&self, hash_number: usize, point: &Point) -> String {
        let mut key_parts: Vec<String> = Vec::new();
        let mut remaining_bits = hash_number;
        for coordinate in point.coordinates() {
            if (remaining_bits % 2) == 1 {
                key_parts.push(
                    displaced_coordinate_key(coordinate, self.precision));
            } else {
                key_parts.push(coordinate_key(coordinate, self.precision));
            }
            remaining_bits /= 2;
        }
        key_parts.join(";")
    }

    //TODO: add fast hash ?
    /// Tries to add a point to the hash.
    /// If a nearby point was already there
    /// returns the nearby point, else adds point and returns it.
    ///
    /// # Example
    ///
    /// ```
    /// use jimn::point::Point;
    /// use jimn::utils::coordinates_hash::CoordinatesHash;
    /// let points = [
    ///     Point::new(0.1231, 0.0),
    ///     Point::new(0.1233, 0.0),
    ///     Point::new(0.1226, 0.0),
    ///     Point::new(0.1220, 0.0),
    /// ];
    /// let mut hash = CoordinatesHash::new(2, 3);
    /// hash.hash_point(&points[0]);
    /// assert_eq!(points[0], hash.hash_point(&points[1]));
    /// assert_eq!(points[0], hash.hash_point(&points[2]));
    /// assert!(points[0] != hash.hash_point(&points[3]));
    /// ```
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
