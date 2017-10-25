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

use num_traits::Float;
use std::iter::once;
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
    hashes: [HashMap<String, f64>; 2],
    precision: usize,
}

impl CoordinatesHash {
    /// Creates a new `CoordinatesHash` with given precision.
    pub fn new(precision: usize) -> CoordinatesHash {
        CoordinatesHash {
            hashes: [HashMap::new(), HashMap::new()],
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


/// The `SquareHash` is a structure allowing identification of nearby points in O(1).
/// Consider a square grid of a given side length.
/// We hash a point into the square containing him. Points hashed in the same square are nearby.
/// Of course two points can also be nearby accross the edge of their respective squares.
/// To also detect them we do not consider a single grid but 4 overlapping grids starting at
/// half side lengths.
/// Each square is identified by a `Point`.
pub struct SquareHash {
    hashes: [HashMap<(String, String), Point>; 4],
    precision: i8,
}

impl SquareHash {
    /// Create a new `SquareHash`.
    /// Grid side's length is 10^precision
    pub fn new(precision: i8) -> Self {
        SquareHash {
            hashes: [
                HashMap::new(),
                HashMap::new(),
                HashMap::new(),
                HashMap::new(),
            ],
            precision,
        }
    }

    /// Returns all 4 squares into which given point hashes.
    pub fn hash_point(&mut self, point: &Point) -> Vec<Point> {
        //TODO: have an iterator instead of returning a vec
        // with negative precision we need to divide the point.
        let (real_precision, real_point) = if self.precision < 0 {
            (
                0,
                Point::new(
                    point.x * 10.0.powi(self.precision as i32),
                    point.y * 10.0.powi(self.precision as i32),
                ),
            )
        } else {
            (self.precision, *point)
        };
        iproduct!(once(coordinate_key), once(displaced_coordinate_key))
            .zip(self.hashes.iter_mut())
            .map(|(hash_functions, hash_map)| {
                let key = (
                    (hash_functions.0)(real_point.x, real_precision as usize),
                    (hash_functions.1)(real_point.y, real_precision as usize),
                );
                *hash_map.entry(key).or_insert(*point)
            })
            .collect()
    }
}
