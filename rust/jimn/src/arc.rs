//! provides the `Arc` class.
use ordered_float::NotNaN;
use std::f64::consts::PI;
use point::Point;
use quadrant::{Shape, Quadrant};
use utils::precision::is_almost;

/// Oriented arc segment.
pub struct Arc {
    /// Starting point
    pub start: Point,
    /// Ending point
    pub end: Point,
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
    fn compute_centers(&self) -> Vec<Point> {
        // we do some geometry to avoid too complex equations.
        // take start as origin
        let translated_end = self.end - self.start;
        // find bisector
        let middle = translated_end / 2.0;
        let bisector_point = middle + translated_end.perpendicular_vector();
        let intersections = line_circle_intersections(&[middle, bisector_point],
                                                      &Point::new(0.0, 0.0),
                                                      self.radius);
        assert!(intersections.len() == 2);
        intersections.iter().map(|i| self.start + i).collect()
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

fn line_circle_intersections(segment: &[Point; 2],
                             center: &Point,
                             radius: NotNaN<f64>)
                             -> Vec<Point> {
    let d = segment[1] - segment[0];
    let c = center - segment[0];
    // segment points are at alpha * d
    // distance(alpha * d, center) = r

    // (xc-alpha*xd)**2 + (yc-alpha*yd)**2 - r**2 = 0

    // xc**2 + alpha**2*xd**2 -2*alpha*xc*xd
    // yc**2 + alpha**2*yd**2 -2*alpha*yc*yd
    // - r**2 = 0
    let a = d.x * d.x + d.y * d.y;
    let b = (c.x * d.x + c.y * d.y) * (-2.0);
    let c = c.x * c.x + c.y * c.y - radius * radius;
    let solutions = solve_quadratic_equation(a, b, c);
    solutions
        .into_iter()
        .map(|s| segment[0] + d * s)
        .collect()
}

fn solve_quadratic_equation(a: NotNaN<f64>, b: NotNaN<f64>, c: NotNaN<f64>) -> Vec<NotNaN<f64>> {
    let delta = b * b - a * c * 4.0;
    if is_almost(delta.abs().sqrt(), 0.0) {
        if is_almost(a, 0.0) {
            Vec::new()
        } else {
            vec![-b / (a * 2.0)]
        }
    } else if delta < NotNaN::new(0.0).unwrap() {
        Vec::new()
    } else {
        vec![(-b - delta.sqrt()) / (a * 2.0),
             (-b + delta.sqrt()) / (a * 2.0)]
    }
}
