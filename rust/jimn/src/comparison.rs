//! Implement key computations for paths.
use std::rc::Rc;
use std::cell::RefCell;
use itertools::repeat_call;
use ordered_float::NotNaN;
use segment::Segment;
use point::Point;
use std::collections::HashMap;

pub trait ProvidesComparisonKey {
    type Key: Copy;
    fn key(&self, point: &Point) -> Self::Key;
}

impl ProvidesComparisonKey for Segment {
    type Key = (NotNaN<f64>, NotNaN<f64>);
    fn key(&self, point: &Point) -> Self::Key {
        let (current_x, current_y) = point.coordinates();
        let x = if self.is_horizontal() {
            current_x
        } else {
            self.horizontal_line_intersection(current_y)
                .expect("computing key for non intersecting segment")
        };
        if current_x > x {
            (x, -self.angle)
        } else {
            (x, self.angle)
        }
    }
}

pub type SegmentIndex = usize;

/// Generator for dynamic keys in sweeping line algorithms.
pub struct KeyGenerator<'a, K: Copy + Ord, T: 'a + AsRef<ProvidesComparisonKey<Key = K>>> {
    /// Where we currently are
    pub current_point: Point,
    /// All paths
    pub paths: &'a [T],
    /// Keys caches for each path
    pub keys_caches: Vec<HashMap<Point, K>>,
}

impl<'a, K: Copy + Ord, T: 'a + AsRef<ProvidesComparisonKey<Key = K>>> KeyGenerator<'a, K, T> {
    /// Create a new `KeyGenerator` for given paths.
    pub fn new(paths: &'a [T]) -> Self {
        KeyGenerator {
            current_point: Default::default(),
            paths,
            keys_caches: repeat_call(HashMap::new).take(paths.len()).collect(),
        }
    }

    /// Find current key for given path.
    pub fn key_for(&self, target: SegmentIndex) -> K {
        if let Some(key) = self.keys_caches[target].get(&self.current_point) {
            *key
        } else {
            self.paths[target].as_ref().key(&self.current_point)
        }
    }
}
