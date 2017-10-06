//! Fast adjusting of nearby coordinates.
//!
//! Provides a `PointsHash` structure which is used to hash
//! nearby points together in O(1).
//! The `PointsHash` is created with a given precision.
//!
//! Considering a distance given by difference between coordinates,
//! we have the following guarantees:
//!
//! * any two coordinates with distance < 5 * 10^-(precision+1) are hashed together.
//! * no coordinates of distance > 10^-precision are hashed together.

use std::collections::HashMap;
//use fnv::FnvHashMap;
use point::Point;
use quadrant::Quadrant;

fn coordinate_key(coordinate: f64, precision: usize) -> String {
    format!("{:.p$}", coordinate, p = precision)
}

fn displaced_coordinate_key(coordinate: f64, precision: usize) -> String {
    coordinate_key(
        coordinate + 5.0 * 10.0_f64.powi(-((precision + 1) as i32)),
        precision,
    )
}

/// a `CoordinatesHash` allows for hashing nearby coordinates together in O(1).
pub struct CoordinatesHash {
    //hashes: Vec<FnvHashMap<String, f64>>,
    hashes: Vec<HashMap<String, f64>>,
    precision: usize,
}

impl CoordinatesHash {
    /// Creates a new `CoordinatesHash` with given precision.
    pub fn new(precision: usize) -> CoordinatesHash {
        CoordinatesHash {
            //hashes: vec![FnvHashMap::default(); 2],
            hashes: vec![HashMap::new(); 2],
            precision: precision,
        }
    }

    /// Hash given coordinate.
    /// If no nearby coordinate in the hash, adds it and returns it
    /// else returns the nearby coordinate.
    pub fn hash_coordinate(&mut self, coordinate: f64) -> f64 {
        let keys = vec![
            coordinate_key(coordinate, self.precision),
            displaced_coordinate_key(coordinate, self.precision),
        ];
        for (hash, key) in self.hashes.iter().zip(keys.iter()) {
            let possible_old_coordinate = hash.get(key);
            if possible_old_coordinate.is_some() {
                return *possible_old_coordinate.unwrap();
            }
        }
        for (hash, key) in self.hashes.iter_mut().zip(keys.into_iter()) {
            hash.insert(key, coordinate);
        }
        coordinate
    }

    /// Lookup a coordinate in the table. Returns nearby coordinate if any or unchanged
    /// value if none. Leaves table unmodified.
    pub fn lookup_coordinate(&self, coordinate: f64) -> f64 {
        let base_key = coordinate_key(coordinate, self.precision);
        let base_lookup = self.hashes[0].get(&base_key);
        match base_lookup {
            Some(x) => *x,
            None => {
                let displaced_key = displaced_coordinate_key(coordinate, self.precision);
                let displaced_lookup = self.hashes[1].get(&displaced_key);
                match displaced_lookup {
                    Some(x) => *x,
                    None => coordinate,
                }
            }
        }
    }
}

/// a `PointsHash` allows for adjusting nearby coordinates O(1).
pub struct PointsHash {
    hashes: [CoordinatesHash; 2],
}

impl PointsHash {
    /// Creates a new `PointsHash` with given precision.
    pub fn new(precision: usize) -> PointsHash {
        PointsHash {
            hashes: [
                CoordinatesHash::new(precision),
                CoordinatesHash::new(precision),
            ],
        }
    }

    /// Tries to add a point to the hash, adjusting coordinates.
    ///
    /// # Example
    ///
    /// ```
    /// use jimn::point::Point;
    /// use jimn::utils::coordinates_hash::PointsHash;
    /// let points = [
    ///     Point::new(0.1231, 0.0),
    ///     Point::new(0.1233, 0.0),
    ///     Point::new(0.1226, 0.0),
    ///     Point::new(0.1220, 0.0),
    /// ];
    /// let mut hash = PointsHash::new(3);
    /// hash.hash_point(&points[0]);
    /// assert_eq!(points[0], hash.hash_point(&points[1]));
    /// assert_eq!(points[0], hash.hash_point(&points[2]));
    /// assert!(points[0] != hash.hash_point(&points[3]));
    /// ```
    pub fn hash_point(&mut self, point: &Point) -> Point {
        Point {
            x: self.hashes[0].hash_coordinate(point.x),
            y: self.hashes[1].hash_coordinate(point.y),
        }
    }

    /// Hash coordinate at given index (0 for x, 1 for y...).
    pub fn hash_coordinate(&mut self, coordinate: f64, index: usize) -> f64 {
        self.hashes[index].hash_coordinate(coordinate)
    }

    /// Returns `Quadrant` delimiting all points we contain.
    /// TODO
    pub fn get_quadrant(&self) -> Quadrant {
        Quadrant::new(2)
    }
}
