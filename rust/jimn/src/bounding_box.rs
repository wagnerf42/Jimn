//! Bound objects with rectangles.
//!
//! Provides `BoundingBox` class to bound objects by rectangles.
//! Allows for example a fast pre-test to know complex shape do not intersect.
//! Boxes are also used for computing display dimensions.
extern crate std;
use std::fmt;
use point::Point;


/// The bounding box structure stores bounds on each coordinate.
/// In 2D this translates to rectangles.
pub struct BoundingBox {
    /// Vector of lower bounds on each coordinate.
    pub min_coordinates: Vec<f64>,
    /// Vector of upper bounds on each coordinate.
    pub max_coordinates: Vec<f64>
}

impl BoundingBox {
    /// Builds a bounding box (unconstrained) in space of given dimension.
    ///
    /// # Examples
    ///
    /// ```
    /// // create an empty box in 2D
    /// use jimn::bounding_box::BoundingBox;
    /// let bbox = BoundingBox::empty_box(2);
    /// ```
    pub fn empty_box(dimension: i64) -> BoundingBox {
        let mut new_box = BoundingBox {
            min_coordinates: vec![],
            max_coordinates: vec![]
        };
        for _ in 0..dimension {
            new_box.min_coordinates.push(std::f64::INFINITY);
            new_box.max_coordinates.push(std::f64::NEG_INFINITY);
        }
        new_box
    }

    /// Adds a point to given box, eventually extending its size.
    ///
    /// # Examples
    ///
    /// ```
    /// use jimn::point::Point;
    /// use jimn::bounding_box::BoundingBox;
    /// let mut bbox = BoundingBox::empty_box(2);
    /// bbox.add_point(&Point::new(2.0, 3.5));
    /// bbox.add_point(&Point::new(1.0, 1.5));
    /// bbox.add_point(&Point::new(2.3, 1.2));
    /// // box is now between point(1.0, 1.2) and point(2.3, 3.5)
    /// assert_eq!(bbox.min_coordinates, vec![1.0, 1.2]);
    /// assert_eq!(bbox.max_coordinates, vec![2.3, 3.5]);
    /// ```
    pub fn add_point(&mut self, point: &Point) {
        let coordinates = point.coordinates();
        for dimension in 0..self.min_coordinates.len() {
            let coordinate = coordinates[dimension];
            if coordinate < self.min_coordinates[dimension] {
                self.min_coordinates[dimension] = coordinate
            }
            if coordinate > self.max_coordinates[dimension] {
                self.max_coordinates[dimension] = coordinate
            }
        }
    }

    /// Updates box by fusing in limits from other.
    /// 
    /// self-> ##                 ###
    ///        ##           ->    ### self after self.update(&other)
    ///         ##                ###
    ///         ## <-other        ###
    pub fn update(&mut self, other: &BoundingBox) {
        for (index, coordinate) in other.min_coordinates.iter().enumerate() {
            if self.min_coordinates[index] > *coordinate {
                self.min_coordinates[index] = *coordinate;
            }
        }
        for (index, coordinate) in other.max_coordinates.iter().enumerate() {
            if self.max_coordinates[index] < *coordinate {
                self.max_coordinates[index] = *coordinate;
            }
        }
    }
}

impl fmt::Display for BoundingBox {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let min_strings: Vec<_> = self.min_coordinates.iter().map(|&c| c.to_string()).collect();
        let max_strings: Vec<_> = self.max_coordinates.iter().map(|&c| c.to_string()).collect();
        write!(f, "[{} ; {}]", min_strings.join(", "), max_strings.join(", "))
    }
}
