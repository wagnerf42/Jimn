//! Bound objects with rectangles.
//!
//! Provides `BoundingBox` class to bound objects by rectangles.
//! Allows for example a fast pre-test to know complex shape do not intersect.
//! Boxes are also used for computing display dimensions.


/// The bounding box structure stores bounds on each coordinate.
/// In 2D this translates to rectangles.
#[derive(Debug)]
pub struct BoundingBox {
    /// Vector of lower bounds on each coordinate.
    pub min_coordinates: Vec<f64>,
    /// Vector of upper bounds on each coordinate.
    pub max_coordinates: Vec<f64>
}

///All points need to implement this Trait.
pub trait IsPoint {
    ///A Point provides coordinates. This is the way to get them.
    fn coordinates(&self) -> Vec<f64>;
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
            min_coordinates: Vec::with_capacity(dimension as usize),
            max_coordinates: Vec::with_capacity(dimension as usize)
        };
        for _ in 0..dimension {
            new_box.min_coordinates.push(::std::f64::INFINITY);
            new_box.max_coordinates.push(::std::f64::NEG_INFINITY);
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
    pub fn add_point<T: IsPoint>(&mut self, point: &T) {
        let coordinates = point.coordinates();
        for (dimension, coordinate) in coordinates.iter().enumerate() {
            if *coordinate < self.min_coordinates[dimension] {
                self.min_coordinates[dimension] = *coordinate
            }
            if *coordinate > self.max_coordinates[dimension] {
                self.max_coordinates[dimension] = *coordinate
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

        for (min_coordinate, coordinate) in self.min_coordinates.iter_mut()
            .zip(other.min_coordinates.iter()) {
            if *min_coordinate > *coordinate {
                *min_coordinate = *coordinate;
            }
        }
        for (max_coordinate, coordinate) in self.max_coordinates.iter_mut()
            .zip(other.max_coordinates.iter()) {
            if *max_coordinate < *coordinate {
                *max_coordinate = *coordinate;
            }
        }
    }
    /// Returns min and max value for given dimension.
    pub fn limits(&self, dimension_index:usize) -> (f64, f64) {
        (self.min_coordinates[dimension_index],
         self.max_coordinates[dimension_index])
    }
    /// Adds border of given size around self.
    pub fn inflate(&mut self, border_size: f64) {
        self.min_coordinates = self.min_coordinates.iter()
            .map(|c| c-border_size).collect();
        self.max_coordinates = self.max_coordinates.iter()
            .map(|c| c+border_size).collect();
    }
}
