pub trait HasCoordinates {
    fn coordinates(&self) -> Vec<f64>;
}

// see https://www.reddit.com/r/rust/comments/3fg0xr/how_do_i_find_the_max_value_in_a_vecf64/
//TODO: types iterables avec flottants au lieu de vecteurs ?
pub fn float_min(vector: &Vec<f64>) -> f64 {
    let mut current_min = vector[0];
    for value in vector {
        if *value < current_min {
            current_min = *value;
        }
    }
    return current_min;
}

pub fn float_max(vector: &Vec<f64>) -> f64 {
    let mut current_max = vector[0];
    for value in vector {
        if *value > current_max {
            current_max = *value;
        }
    }
    return current_max;
}

pub mod bounding_box;
pub mod point;
pub mod tycat;
