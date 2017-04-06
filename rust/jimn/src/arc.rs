//! provides the `Arc` class.
use ordered_float::NotNaN;
use std::f64::consts::PI;
use point::Point;
use quadrant::{Shape, Quadrant};
use utils::precision::is_almost;

/// Oriented arc segment.
pub struct Arc {
    start: Point,
    end: Point,
    center: Point,
    radius: NotNaN<f64>,
}

impl Arc {
    /// Create a new arc.
    pub fn new<T: Into<NotNaN<f64>>>(start: Point, end: Point, center: Point, radius: T) -> Arc {
        let mut arc = Arc {
            start: start,
            end: end,
            center: center,
            radius: radius.into(),
        };
        if !(is_almost(arc.center.distance_to(&arc.start), arc.radius) &&
             is_almost(arc.center.distance_to(&arc.end), arc.radius)) {
            arc.adjust_center();
        }
        arc
    }

    /// Given center was not completely right, move it slightly.
    /// This can happen for example when endpoints have been rounded.
    fn adjust_center(&mut self) {
        self.center = *self.compute_centers()
                           .iter()
                           .min_by_key(|c| c.distance_to(&self.center))
                           .unwrap();
    }

    /// Return array of the two centers we could have.
    fn compute_centers(&self) -> [Point; 2] {
        // we do some geometry to avoid too complex equations.
        // take start as origin
        let translated_end = self.end - self.start;
        // find bisector
        let middle = translated_end / 2.0;
        let bisector_point = middle + translated_end.perpendicular_vector();
        unimplemented!()
    }

    /// Return normalized angle of points with center.
    pub fn angle(&self) -> NotNaN<f64> {
        (self.center.angle_with(&self.start) - self.center.angle_with(&self.end) + 2.0 * PI) %
        (2.0 * PI)
    }
}

impl Shape for Arc {
    /// Return quadrant for display (also display center).
    /// This quadrant is not tight.
    fn get_quadrant(&self) -> Quadrant {
        Quadrant {
            min_coordinates: vec![self.center.x - self.radius, self.center.y - self.radius],
            max_coordinates: vec![self.center.x + self.radius, self.center.y + self.radius],
        }
    }

    fn svg_string(&self) -> String {
        let center_string = self.center.svg_string();
        // go always for the small arc
        let sweep_flag = if self.angle() > NotNaN::new(PI).unwrap() {
            1
        } else {
            0
        };
        let arc_string = format!("<path d=\"M{},{} A{},{} 0 0,{} {},{}\" fill=\"none\"/>",
                                 self.start.x,
                                 self.start.y,
                                 self.radius,
                                 self.radius,
                                 sweep_flag,
                                 self.end.x,
                                 self.end.y);
        center_string + &arc_string
    }
}
