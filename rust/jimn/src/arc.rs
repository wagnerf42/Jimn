//! provides the `Arc` class.
use std::iter::{empty, once};
use std::f64::consts::{FRAC_PI_2, PI};
use {ElementaryPath, Point, Segment};
use quadrant::{Quadrant, Shape};
use utils::precision::is_almost;
use utils::coordinates_hash::PointsHash;
use utils::ArrayMap;

/// Oriented arc segment.
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub struct Arc {
    /// Starting point
    pub start: Point,
    /// Ending point
    pub end: Point,
    /// Center
    pub center: Point,
    /// Radius
    pub radius: f64,
}

impl Arc {
    /// Create a new arc.
    pub fn new(start: Point, end: Point, center: Point, radius: f64) -> Arc {
        let mut arc = Arc {
            start,
            end,
            center,
            radius,
        };
        if !(is_almost(arc.center.distance_to(&arc.start), arc.radius)
            && is_almost(arc.center.distance_to(&arc.end), arc.radius))
        {
            arc.adjust_center();
        }
        arc
    }

    /// Given center was not completely right, move it slightly.
    /// This can happen for example when endpoints have been rounded.
    fn adjust_center(&mut self) {
        self.center = *self.compute_centers()
            .iter()
            .min_by(|c1, c2| {
                c1.distance_to(&self.center)
                    .partial_cmp(&c2.distance_to(&self.center))
                    .unwrap()
            })
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
        let line = Segment::new(middle, bisector_point);
        let origin = Point::new(0.0, 0.0);
        let intersections = line_circle_intersections(&line, &origin, self.radius);
        let centers: Vec<_> = intersections.map(|i| self.start + i).collect();
        assert_eq!(centers.len(), 2);
        centers
    }

    /// Return normalized angle of points with center.
    pub fn angle(&self) -> f64 {
        (self.center.angle_with(&self.start) - self.center.angle_with(&self.end) + 2.0 * PI)
            % (2.0 * PI)
    }

    /// Return the arc's length.
    pub fn length(&self) -> f64 {
        let angle = self.angle();
        if angle > PI {
            ((2.0 * PI) - angle) * self.radius
        } else {
            angle * self.radius
        }
    }

    /// Do we contain given point ?
    pub fn contains(&self, point: &Point) -> bool {
        if self.start.is_almost(point) || self.end.is_almost(point) {
            true
        } else if is_almost(self.center.distance_to(point), self.radius) {
            self.contains_circle_point(point)
        } else {
            false
        }
    }

    /// Do we contain given point on circle but not as endpoint ?
    pub fn strictly_contains(&self, point: &Point) -> bool {
        if self.start.is_almost(point) || self.end.is_almost(point) {
            false
        } else {
            self.contains_circle_point(point)
        }
    }

    /// Do we contain given point which is on our circle ?
    pub fn contains_circle_point(&self, point: &Point) -> bool {
        let s = Segment::new(self.start, self.end);
        let s2 = Segment::new(self.center, *point);
        s.intersection_with(&s2).is_some()
    }

    /// Intersect ourselves with horizontal line at given y.
    /// pre-condition: there is exactly one intersection
    ///
    /// # Example
    /// ```
    /// use jimn::{Point, Arc};
    /// let arc = Arc::new(Point::new(1.0, 0.0), Point::new(0.0, -1.0), Point::new(0.0, 0.0), 1.0);
    /// let half_coordinate = 1.0/(2.0 as f64).sqrt();
    /// let half_point = arc.horizontal_line_intersection(-half_coordinate);
    /// assert!(half_point.is_almost(&Point::new(half_coordinate, -half_coordinate)));
    /// ```
    pub fn horizontal_line_intersection(&self, y: f64) -> Point {
        // we use pythagoras
        let side_length = (y - self.center.y).abs();
        if is_almost(side_length, self.radius) {
            return Point::new(self.center.x, y);
        }
        if side_length > self.radius {
            panic!("no arc hline intersection");
        }
        let other_side_length = (self.radius * self.radius - side_length * side_length).sqrt();
        let candidate_point = Point::new(self.center.x - other_side_length, y);
        if self.contains_circle_point(&candidate_point) {
            candidate_point
        } else {
            let candidate_point2 = Point::new(self.center.x + other_side_length, y);
            if self.contains_circle_point(&candidate_point2) {
                candidate_point2
            } else {
                panic!("no arc hline");
            }
        }
    }

    /// Return angle for tangent at given point.
    /// pre-condition: we contain given point.
    ///
    /// # Example
    ///
    /// ```
    /// use jimn::{Point, Arc};
    /// use std::f64::consts::PI;
    /// use jimn::utils::precision::is_almost;
    ///
    /// let arc = Arc::new(Point::new(1.0, 0.0), Point::new(0.0, -1.0), Point::new(0.0, 0.0), 1.0);
    /// let half_coordinate = 1.0/(2.0 as f64).sqrt();
    /// assert!(is_almost(arc.tangent_angle(&Point::new(half_coordinate, -half_coordinate)),
    ///                   PI/4.0));
    /// ```
    pub fn tangent_angle(&self, tangent_point: &Point) -> f64 {
        let base_angle = self.center.angle_with(tangent_point);
        (base_angle + FRAC_PI_2) % PI
    }

    /// Split given `Arc` in possible two so that for any given y, each arc
    /// only has one point.
    /// Does not return anything if arc requires no splitting.
    pub fn split_for_unique_y(&self, rounder: &mut PointsHash) -> Vec<ElementaryPath> {
        for direction in &[1.0f64, -1.0f64] {
            let extremum = self.center + Point::new(0.0, self.radius * *direction);
            if self.strictly_contains(&extremum) {
                let rounded_extremum = rounder.hash_point(&extremum);
                return vec![
                    ElementaryPath::Arc(Arc::new(
                        self.start,
                        rounded_extremum,
                        self.center,
                        self.radius,
                    )),
                    ElementaryPath::Arc(Arc::new(
                        rounded_extremum,
                        self.end,
                        self.center,
                        self.radius,
                    )),
                ];
            }
        }
        vec![ElementaryPath::Arc(*self)]
    }

    /// Iterate on all points obtained when intersecting with given Arc.
    pub fn intersections_with_arc<'a>(
        &'a self,
        other: &'a Self,
    ) -> impl Iterator<Item = Point> + 'a {
        circles_intersections(&self.center, &other.center, self.radius, other.radius)
            .filter(move |p| self.contains_circle_point(p) && other.contains_circle_point(p))
    }

    /// Iterate on all points obtained when intersecting with given Segment.
    pub fn intersections_with_segment<'a>(
        &'a self,
        other: &'a Segment,
    ) -> impl Iterator<Item = Point> + 'a {
        line_circle_intersections(other, &self.center, self.radius)
            .filter(move |p| self.contains_circle_point(p) && other.contains(p))
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
        let sweep_flag = if self.angle() > PI { 1 } else { 0 };
        let arc_string = format!(
            "<path d=\"M{},{} A{},{} 0 0,{} {},{}\" fill=\"none\"/>",
            self.start.x,
            self.start.y,
            self.radius,
            self.radius,
            sweep_flag,
            self.end.x,
            self.end.y
        );
        // now draw a small arrow indicating orientation
        let mut middle_angle =
            (self.center.angle_with(&self.start) + self.center.angle_with(&self.end)) / 2.0;
        // we need to figure out where is the middle point between two candidates
        let angles = [middle_angle, middle_angle + PI];
        let possible_points =
            angles.map(|a| self.center + Point::new(a.cos(), a.sin()) * self.radius);
        let distances = possible_points.map(|p| self.start.distance_to(p));
        let tangent_point = if distances[0] < distances[1] {
            possible_points[0]
        } else {
            middle_angle += PI;
            possible_points[1]
        };
        let tangent_angle = if sweep_flag == 1 {
            middle_angle + FRAC_PI_2
        } else {
            middle_angle - FRAC_PI_2
        };
        let arrow_string = format!(
            "<use xlink:href=\"#a\" x=\"{}\" y=\"{}\" transform=\"rotate({} {} {})\"/>",
            tangent_point.x,
            tangent_point.y,
            tangent_angle * 180.0 / PI,
            tangent_point.x,
            tangent_point.y
        );
        center_string + &arc_string + &arrow_string
    }
}

fn line_circle_intersections<'a>(
    segment: &'a Segment,
    center: &'a Point,
    radius: f64,
) -> impl Iterator<Item = Point> + 'a {
    let d = segment.end - segment.start;
    let c = center - segment.start;
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
    solutions.into_iter().map(move |s| segment.start + d * s)
}

fn solve_quadratic_equation(a: f64, b: f64, c: f64) -> Vec<f64> {
    let delta = b * b - a * c * 4.0;
    if is_almost(delta.abs().sqrt(), 0.0) {
        if is_almost(a, 0.0) {
            Vec::new()
        } else {
            vec![-b / (a * 2.0)]
        }
    } else if delta < 0.0 {
        Vec::new()
    } else {
        vec![
            (-b - delta.sqrt()) / (a * 2.0),
            (-b + delta.sqrt()) / (a * 2.0),
        ]
    }
}

fn circles_intersections(c1: &Point, c2: &Point, r1: f64, r2: f64) -> Box<Iterator<Item = Point>> {
    // TODO: should we unbox everything
    // I just solved all equations to end up with this.
    let d = c1.distance_to(c2);
    if is_almost(d, 0.0) {
        Box::new(empty()) // common center
    } else {
        let (x1, y1) = c1.coordinates();
        let (x2, y2) = c2.coordinates();
        let l = if is_almost(r1, r2) {
            d / 2.0
        } else {
            (r1 * r1 - r2 * r2) / (d * 2.0) + d / 2.0
        };

        if is_almost(r1, l) {
            // only one intersection
            Box::new(once(Point::new(
                l / d * (x2 - x1) + x1,
                l / d * (y2 - y1) + y1,
            )))
        } else if (r1 < l) || (r1.abs() < l.abs()) {
            Box::new(empty()) // too far away
        } else {
            let h = (r1 * r1 - l * l).sqrt();
            let p1 = Point::new(
                l / d * (x2 - x1) + h / d * (y2 - y1) + x1,
                l / d * (y2 - y1) - h / d * (x2 - x1) + y1,
            );
            let p2 = Point::new(
                l / d * (x2 - x1) - h / d * (y2 - y1) + x1,
                l / d * (y2 - y1) + h / d * (x2 - x1) + y1,
            );
            if p1.is_almost(&p2) {
                Box::new(once(p1))
            } else {
                Box::new(once(p1).chain(once(p2)))
            }
        }
    }
}
