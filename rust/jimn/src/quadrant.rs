//! Bound objects with rectangles.
//!
//! Provides `Quadrant` class to bound objects by rectangles.
//! Allows for example a fast pre-test to know complex shape do not intersect.
//! Quadrants are also used for computing display dimensions.

use point::Point;
use segment::Segment;
use utils::coordinates_hash::PointsHash;

/// The `Quadrant` structure stores bounds on each coordinate.
/// In 2D this translates to rectangles.
#[derive(Debug, Clone)]
pub struct Quadrant {
    /// Vector of lower bounds on each coordinate.
    pub min_coordinates: Vec<f64>,
    /// Vector of upper bounds on each coordinate.
    pub max_coordinates: Vec<f64>,
}

/// Any displayable `Shape` is enclosed in a quadrant.
pub trait Shape {
    /// Gets min quadrant in which we are enclosed.
    fn get_quadrant(&self) -> Quadrant;
    /// Returns svg code displaying it.
    fn svg_string(&self) -> String;
}

impl<'a, S: Shape> Shape for &'a S {
    fn get_quadrant(&self) -> Quadrant {
        (*self).get_quadrant()
    }
    fn svg_string(&self) -> String {
        (*self).svg_string()
    }
}

impl Quadrant {
    /// Builds a `Quadrant` (unconstrained) in space of given dimension.
    ///
    /// # Examples
    ///
    /// ```
    /// // create an empty quadrant in 2D
    /// use jimn::quadrant::Quadrant;
    /// let quadrant = Quadrant::new(2);
    /// ```
    pub fn new(dimension: usize) -> Quadrant {
        Quadrant {
            min_coordinates: vec![::std::f64::INFINITY; dimension],
            max_coordinates: vec![::std::f64::NEG_INFINITY; dimension],
        }
    }

    /// Return dimensions (width, height, ...) of given `Quadrant`.
    pub fn dimensions(&self) -> Vec<f64> {
        self.min_coordinates
            .iter()
            .zip(self.max_coordinates.iter())
            .map(|(&a, &b)| (b - a).abs())
            .collect()
    }

    /// Updates `Quadrant` by fusing in limits from other.
    ///
    /// self-> ##                 ###
    ///        ##           ->    ### self after self.update(&other)
    ///                           ###
    ///         ##                ###
    ///         ## <-other        ###
    pub fn update(&mut self, other: &Quadrant) {
        for (min_coordinate, coordinate) in self.min_coordinates
            .iter_mut()
            .zip(other.min_coordinates.iter())
        {
            if *min_coordinate > *coordinate {
                *min_coordinate = *coordinate;
            }
        }
        for (max_coordinate, coordinate) in self.max_coordinates
            .iter_mut()
            .zip(other.max_coordinates.iter())
        {
            if *max_coordinate < *coordinate {
                *max_coordinate = *coordinate;
            }
        }
    }

    /// Returns min and max value for given dimension.
    pub fn limits(&self, dimension_index: usize) -> (f64, f64) {
        (
            self.min_coordinates[dimension_index],
            self.max_coordinates[dimension_index],
        )
    }

    /// Adds border of given size around self.
    pub fn inflate(&mut self, border_size: f64) {
        self.min_coordinates = self.min_coordinates
            .iter()
            .map(|&c| c - border_size)
            .collect();
        self.max_coordinates = self.max_coordinates
            .iter()
            .map(|&c| c + border_size)
            .collect();
    }

    /// Adds quadrant around given shape to ourselves.
    pub fn add<T: Shape>(&mut self, shape: &T) {
        let quadrant = shape.get_quadrant();
        self.update(&quadrant)
    }

    /// Return 2d segments bounding us.
    /// Note this only considers the first 2 dimensions.
    pub fn segments(&self, rounder: &mut PointsHash) -> Vec<Segment> {
        let points = vec![
            Point::new(self.min_coordinates[0], self.min_coordinates[1]),
            Point::new(self.min_coordinates[0], self.max_coordinates[1]),
            Point::new(self.max_coordinates[0], self.max_coordinates[1]),
            Point::new(self.max_coordinates[0], self.min_coordinates[1]),
        ];
        let rpoints: Vec<_> = points.iter().map(|p| rounder.hash_point(p)).collect();
        rpoints
            .iter()
            .zip(rpoints.iter().cycle().skip(1))
            .map(|(p1, p2)| Segment::new(*p1, *p2))
            .collect()
    }

    /// Return max dimension.
    pub fn size(&self) -> f64 {
        self.dimensions()
            .iter()
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .cloned()
            .unwrap()
    }
}
