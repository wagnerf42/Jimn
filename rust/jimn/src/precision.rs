//! precision related help functions
//! geometry can be very tricky the use of floating point numbers easily
//! leads to rounding errors.
//! we keep here a set of helper functions to try to reduce impact of
//! errors.
const PRECISION: i32 = 6;

//TODO: avoid recomputing limit over and over again
/// return if two given coordinates are nearby (absolute difference is
/// less than jimn's default precision limit).
pub fn is_almost(coordinate1: f64, coordinate2: f64) -> bool {
    (coordinate1 - coordinate2).abs() < 10.0f64.powi(-PRECISION)
}
