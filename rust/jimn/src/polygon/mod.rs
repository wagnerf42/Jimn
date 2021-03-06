//! Polygons.
//! Provides `Polygon` structure.

use std::iter::once;
use std::f64;

use quadrant::{Quadrant, Shape};
use point::Point;
use Segment;
use utils::Identifiable;
use utils::precision::is_almost;

pub use self::polygon_builder::build_polygons;
mod polygon_builder;

/// Oriented polygons.
#[derive(Clone, Debug)]
pub struct Polygon {
    /// Vector of all points forming the edge of the polygon.
    pub points: Vec<Point>,
}

impl Identifiable for Polygon {}
/// Return an empty polygon as default
impl Default for Polygon {
    fn default() -> Self {
        Polygon { points: Vec::new() }
    }
}

impl<'a> Polygon {
    /// Create polygon out of given points vector.
    pub fn new(points: Vec<Point>) -> Polygon {
        Polygon { points: points }
    }

    /// Return iterator on all our segments.
    pub fn segments(&'a self) -> impl Iterator<Item = Segment> + 'a {
        self.points
            .iter()
            .zip(self.points.iter().cycle().skip(1))
            .map(|(&p1, &p2)| Segment::new(p1, p2))
    }

    /// Returns area taken by polygon.
    /// Negative or Positive depending on orientation.
    pub fn area(&self) -> f64 {
        self.points
            .iter()
            .zip(self.points.iter().cycle().skip(1))
            .map(|(p1, p2)| p1.cross_product(p2))
            .sum()
    }

    /// Returns if polygon is oriented clockwise (with respect to svg
    /// orientation)
    pub fn is_oriented_clockwise(&self) -> bool {
        let area = self.area();
        assert!(!is_almost(area, 0.0)); // flat or crossing polygon
        area > 0.0
    }

    /// Return a point strictly inside us (not on edge) in O(n) time.
    pub fn inner_point(&self) -> Point {
        // Take a y we intersect with.
        let mut ymin: f64 = f64::INFINITY;
        let mut ymax: f64 = f64::NEG_INFINITY;
        for point in &self.points {
            if point.y < ymin {
                ymin = point.y;
            }
            if point.y > ymax {
                ymax = point.y;
            }
        }
        let y = (ymin + ymax) / 2.0;
        // now look at all segments intersecting with this y and take the two leftmost.
        let (x1, x2) = self.segments()
            .filter_map(|segment| {
                let (p1, p2) = if segment.start.y > segment.end.y {
                    (segment.end, segment.start)
                } else {
                    (segment.start, segment.end)
                };
                if p1.y == p2.y {
                    if p1.y == y {
                        Some(p1.x)
                    } else {
                        None
                    }
                } else {
                    if p1.y <= y && p2.y >= y {
                        Some(segment.horizontal_line_intersection(y))
                    } else {
                        None
                    }
                }
            })
            .fold((f64::INFINITY, f64::INFINITY), |(x1, x2), x| {
                if x < x1 {
                    (x, x1)
                } else if x < x2 {
                    (x1, x)
                } else {
                    (x1, x2)
                }
            });
        Point::new((x1 + x2) / 2.0, y)
    }

    /// Simplifies polygon by removing points
    /// without losing too much precision.
    ///
    /// # Example
    /// ```
    /// use jimn::point::Point;
    /// use jimn::polygon::Polygon;
    /// //note: you can add some display! to visualize the example.
    ///
    /// let complex_polygon = Polygon::new(
    ///     vec![
    ///     Point::new(-1.5, 0.2071000039577484),
    ///     Point::new(-1.29497096657753, 0.7020999744534493),
    ///     Point::new(-1.2928999662399292, 0.707099974155426),
    ///     Point::new(-1.1728129839897157, 0.9970709997415542),
    ///     Point::new(-1.1715999841690063, 1.0),
    ///     Point::new(-1.1728129839897157, 1.0029289996623993),
    ///     Point::new(-1.2928999662399292, 1.2928999662399292),
    ///     Point::new(-1.0029289996623993, 1.1728129839897157),
    ///     Point::new(-1.0, 1.1715999841690063),
    ///     Point::new(-0.7100289744138718, 1.2916869664192199),
    ///     Point::new(-0.707099974155426, 1.2928999662399292),
    ///     Point::new(-0.2121000036597252, 1.4979289996623992),
    ///     Point::new(-0.2071000039577484, 1.5),
    ///     Point::new(-0.002071000039577484, 1.005),
    ///     Point::new(0.0, 1.0),
    ///     Point::new(0.20502900391817092, 1.495),
    ///     Point::new(0.2071000039577484, 1.5),
    ///     Point::new(0.7020999744534493, 1.29497096657753),
    ///     Point::new(0.707099974155426, 1.2928999662399292),
    ///     Point::new(0.9970709997415542, 1.1728129839897157),
    ///     Point::new(1.0, 1.1715999841690063),
    ///     Point::new(1.2899709665775299, 1.2916869664192199),
    ///     Point::new(1.2928999662399292, 1.2928999662399292),
    ///     Point::new(1.2916869664192199, 1.2899709665775299),
    ///     Point::new(1.1715999841690063, 1.0),
    ///     Point::new(1.2916869664192199, 0.7100289744138718),
    ///     Point::new(1.2928999662399292, 0.707099974155426),
    ///     Point::new(1.4979289996623992, 0.2121000036597252),
    ///     Point::new(1.5, 0.2071000039577484),
    ///     Point::new(1.495, 0.20502900391817092),
    ///     Point::new(1.0, 0.0),
    ///     Point::new(1.495, -0.20502900391817092),
    ///     Point::new(1.5, -0.2071000039577484),
    ///     Point::new(1.4979289996623992, -0.2121000036597252),
    ///     Point::new(1.2928999662399292, -0.707099974155426),
    ///     Point::new(1.2916869664192199, -0.7100289744138718),
    ///     Point::new(1.1715999841690063, -1.0),
    ///     Point::new(1.2916869664192199, -1.2899709665775299),
    ///     Point::new(1.2928999662399292, -1.2928999662399292),
    ///     Point::new(1.2899709665775299, -1.2916869664192199),
    ///     Point::new(1.0, -1.1715999841690063),
    ///     Point::new(0.9970709997415542, -1.1728129839897157),
    ///     Point::new(0.707099974155426, -1.2928999662399292),
    ///     Point::new(0.7020999744534493, -1.29497096657753),
    ///     Point::new(0.2071000039577484, -1.5),
    ///     Point::new(0.20502900391817092, -1.495),
    ///     Point::new(0.0, -1.0),
    ///     Point::new(-0.002071000039577484, -1.005),
    ///     Point::new(-0.2071000039577484, -1.5),
    ///     Point::new(-0.2121000036597252, -1.4979289996623992),
    ///     Point::new(-0.707099974155426, -1.2928999662399292),
    ///     Point::new(-0.7100289744138718, -1.2916869664192199),
    ///     Point::new(-1.0, -1.1715999841690063),
    ///     Point::new(-1.0029289996623993, -1.1728129839897157),
    ///     Point::new(-1.2928999662399292, -1.2928999662399292),
    ///     Point::new(-1.1728129839897157, -1.0029289996623993),
    ///     Point::new(-1.1715999841690063, -1.0),
    ///     Point::new(-1.1728129839897157, -0.9970709997415542),
    ///     Point::new(-1.2928999662399292, -0.707099974155426),
    ///     Point::new(-1.29497096657753, -0.7020999744534493),
    ///     Point::new(-1.5, -0.2071000039577484),
    ///     Point::new(-1.005, -0.002071000039577484),
    ///     Point::new(-1.0, 0.0),
    ///     Point::new(-1.005, 0.002071000039577484)
    ///         ]);
    /// let simple_polygon = complex_polygon.simplify();
    /// assert!(simple_polygon.points.len() == 24);
    /// ```
    pub fn simplify(&self) -> Polygon {
        //triangle area
        fn area(p1: &Point, p2: &Point, p3: &Point) -> f64 {
            (p1.cross_product(p2) + p2.cross_product(p3) + p3.cross_product(p1)).abs() / 2.0
        }

        //remove all small triangles
        //when looping on 3 consecutive points
        let new_points: Vec<Point> = self.points
            .iter()
            .zip(self.points.iter().cycle().skip(1))
            .zip(self.points.iter().cycle().skip(2))
            .filter_map(|((p1, p2), p3)| {
                if area(p1, p2, p3) < 0.000001 {
                    None
                } else {
                    Some(*p2)
                }
            })
            .collect();
        //now remove aligned points
        let final_points: Vec<Point> = new_points
            .iter()
            .zip(new_points.iter().cycle().skip(1))
            .zip(new_points.iter().cycle().skip(2))
            .filter_map(|((p1, p2), p3)| {
                if p1.is_aligned_with(p2, p3) {
                    None
                } else {
                    Some(*p2)
                }
            })
            .collect();
        assert!(final_points.len() > 2);
        Polygon::new(final_points)
    }
}

impl Shape for Polygon {
    fn get_quadrant(&self) -> Quadrant {
        let mut quadrant = Quadrant::new(2);
        for point in &self.points {
            quadrant.add(point);
        }
        quadrant
    }

    fn svg_string(&self) -> String {
        if self.points.is_empty() {
            String::new()
        } else {
            once("<polygon points=\"".to_string())
                .chain(self.points.iter().map(|p| format!(" {},{}", p.x, p.y)))
                .chain(once("\" opacity=\"0.5\"/>".to_string()))
                .collect()
        }
    }
}

/// Return a square polygon with top left corner at given (x,y) and side of l.
pub fn square(x: f64, y: f64, l: f64) -> Polygon {
    Polygon {
        points: vec![
            Point::new(x, y),
            Point::new(x + l, y),
            Point::new(x + l, y + l),
            Point::new(x, y + l),
        ],
    }
}
