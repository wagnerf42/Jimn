//! Bound objects with rectangles.
//!
//! Provides `Quadrant` class to bound objects by rectangles.
//! Allows for example a fast pre-test to know complex shape do not intersect.
//! Quadrants are also used for computing display dimensions.

use ordered_float::NotNaN;
use point::Point;

/// The bounding box structure stores bounds on each coordinate.
/// In 2D this translates to rectangles.
#[derive(Debug)]
pub struct Quadrant {
    /// Vector of lower bounds on each coordinate.
    pub min_coordinates: Vec<NotNaN<f64>>,
    /// Vector of upper bounds on each coordinate.
    pub max_coordinates: Vec<NotNaN<f64>>
}

impl Quadrant {
    /// Builds a `Quadrant` (unconstrained) in space of given dimension.
    ///
    /// # Examples
    ///
    /// ```
    /// // create an empty quadrant in 2D
    /// use jimn::quadrant::Quadrant;
    /// let bbox = Quadrant::new(2);
    /// ```
    pub fn new(dimension: usize) -> Quadrant {
        Quadrant {
            min_coordinates:
                vec![NotNaN::new(::std::f64::INFINITY).unwrap(); dimension],
            max_coordinates:
                vec![NotNaN::new(::std::f64::NEG_INFINITY).unwrap(); dimension]
        }
    }

    /// Updates `Quadrant` by fusing in limits from other.
    ///
    /// self-> ##                 ###
    ///        ##           ->    ### self after self.update(&other)
    ///                           ###
    ///         ##                ###
    ///         ## <-other        ###
    pub fn update(&mut self, other: &Quadrant) {

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
    pub fn limits(&self, dimension_index:usize)
        -> (NotNaN<f64>, NotNaN<f64>) {
            (self.min_coordinates[dimension_index],
             self.max_coordinates[dimension_index])
        }

    /// Adds border of given size around self.
    pub fn inflate<T:Into<NotNaN<f64>>>(&mut self, border_size: T) {
        let border = border_size.into();
        self.min_coordinates = self.min_coordinates.iter()
            .map(|&c| c-border).collect();
        self.max_coordinates = self.max_coordinates.iter()
            .map(|&c| c+border).collect();
    }

    /// Adds given point to ourselves.
    pub fn add_point(&mut self, point: &Point) {
        let quadrant = point.get_quadrant();
        self.update(&quadrant)
    }
}
