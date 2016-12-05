//! Precision related help functions.
//! Geometry can be very tricky the use of floating point numbers easily
//! leads to rounding errors.
//! We keep here a set of helper functions to try to reduce impact of
//! errors.
const PRECISION: i32 = 6;

//TODO: avoid recomputing limit over and over again
/// Returns if two given coordinates are nearby (absolute difference is
/// less than jimn's default precision limit).
pub fn is_almost<T:Into<f64>>(coordinate1: T, coordinate2: T) -> bool {
    (coordinate1.into() - coordinate2.into()).abs() < 10.0f64.powi(-PRECISION)
}
