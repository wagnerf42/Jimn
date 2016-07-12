pub trait HasCoordinates {
    fn coordinates(&self) -> Vec<f64>;
}

pub mod bounding_box;
pub mod point;
pub mod tycat;
