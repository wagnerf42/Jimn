const PRECISION: i32 = 6;

//TODO: avoid recomputing limit over and over again
pub fn is_almost(coordinate1: f64, coordinate2: f64) -> bool {
    (coordinate1 - coordinate2).abs() < 10.0f64.powi(-PRECISION)
}

//TODO: how to change precision in the format ?
pub fn coordinate_key(coordinate: f64) -> String {
    format!("{:.6}", coordinate)
}

pub fn displaced_coordinate_key(coordinate: f64) -> String {
    coordinate_key(10.0f64.powi(-PRECISION)+ coordinate)
}
