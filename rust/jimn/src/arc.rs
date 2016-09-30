//! Arcs.
//! Provides `Arc` structure storing arcs segments (less than half circle).
use bounding_box::BoundingBox;
use point::Point;
use tycat::{Displayer, Displayable};
use elementary_path::ElementaryPath;
use std::io::Write;
use utils::precision::is_almost;
use utils::Identifiable;

/// Oriented circular arc structure.
/// Only stores arcs smaller than half circles.
#[derive(Debug)]
pub struct Arc {
    points: [Point; 2],
    radius: f64,
    center: Point,
    reversed_direction: bool
}

impl Identifiable for Arc {}
impl Arc {
    /// Returns a new arc with given radius, going from start to end.
    /// The `reversed` boolean indicates orientation.
    pub fn new(radius: f64,
               start: Point, end: Point,
               center: Point,
               reversed: bool) -> Arc {
        // first, arguments are coherent
        if cfg!(debug_assertions) {
            assert!(!(start.is_almost(&end)));
            assert!(is_almost(center.distance_to(&start), radius));
            // TODO: check orientation
        }
        Arc {
            points: [start, end],
            radius: radius,
            center: center,
            reversed_direction: reversed
        }
    }
    /// Returns `Arc` going from end to start (on same path).
    pub fn reverse(&self) -> Arc {
        Arc {
            points: [self.points[1], self.points[0]],
            radius: self.radius,
            center: self.center,
            reversed_direction: !self.reversed_direction
        }
    }
}

impl ElementaryPath for Arc {
    fn points(&self) -> (Point, Point) {
        (self.points[0], self.points[1])
    }
}

impl Displayable for Arc {
    fn get_bounding_box(&self) -> BoundingBox {
        let mut bbox = BoundingBox::empty_box(2);
        for point in &self.points {
            bbox.add_point(point);
        }
        bbox
    }
    
    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        // display first point for viewer to know orientation
        self.points[0].save_svg_content(displayer, color);
        // display svg arc
        let svg_coordinates:Vec<Vec<f64>> = self.points.iter()
            .map(|&p| displayer.convert_coordinates(p.coordinates()))
            .collect();
        let sweep_flag = if self.reversed_direction { 0 } else { 1 };
        let stretched_radius = self.radius * displayer.stretch;
        self.center.save_svg_content(displayer, color);
        writeln!(displayer.svg_file,
               "<path d=\"M{},{} A{},{} 0 0,{} {},{}\" \
               fill=\"none\" stroke=\"{}\" opacity=\"0.5\" \
               stroke-width=\"{}\"/>",
               svg_coordinates[0][0], svg_coordinates[0][1],
               stretched_radius, stretched_radius,
               sweep_flag,
               svg_coordinates[1][0], svg_coordinates[1][1],
               color, displayer.stroke_width
               ).expect("cannot write svg file, disk full ?");
    }
}
