extern crate jimn;
use jimn::utils::coordinates_hash::CoordinatesHash;
use jimn::point::Point;

fn main() {
    let points = [
        Point::new(0.123456789, 0.0),
        Point::new(0.123456788, 0.0),
        Point::new(0.123456778, 0.0),
        Point::new(0.123456678, 0.0),
        Point::new(0.123455678, 0.0),
        Point::new(0.123445678, 0.0),
        Point::new(0.123345678, 0.0),
        Point::new(0.122345678, 0.0),
        Point::new(0.112345678, 0.0),
        Point::new(0.012345678, 0.0),
    ];
    let mut hash = CoordinatesHash::new(2, 3);
    for point in &points {
        let hashed_point = hash.hash_point(point);
        println!("{:?} hashes to {:?}", point, hashed_point);
    }
}
