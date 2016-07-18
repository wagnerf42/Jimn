pub trait HasCoordinates {
    fn coordinates(&self) -> Vec<f64>;
}

//TODO: types iterables avec flottants au lieu de vecteurs ?
//TODO: passer le vec par reference ou non ?
pub fn float_min(vector: &Vec<f64>) -> f64 {
    let mut current_min = vector[0];
    for value in vector {
        if *value < current_min {
            current_min = *value;
        }
    }
    return current_min;
}

pub mod bounding_box;
pub mod point;
pub mod tycat;
