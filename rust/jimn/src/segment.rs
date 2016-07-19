use bounding_box::BoundingBox;
use point::Point;
use tycat::Displayer;
use tycat::Displayable;
use std::io::Write;

pub struct Segment {
    points: [Point; 2]
}

impl Segment {
    pub fn new(start: &Point, end: &Point) -> Segment {
        Segment {
            points: [*start, *end]
        }
    }
}

impl Displayable for Segment {
    fn get_bounding_box(&self) -> BoundingBox {
        let mut bbox = BoundingBox::empty_box(2);
        for point in &self.points {
            bbox.add_point(point);
        }
        return bbox
    }

    fn save_svg_content(&self, displayer: &mut Displayer, color: &str) {
        let svg_coordinates:Vec<Vec<f64>> = self.points.iter()
            .map(|&p| displayer.convert_coordinates(p.coordinates()))
            .collect();
        write!(displayer.svg_file, "<line x1=\"{}\" y1=\"{}\" \
               x2=\"{}\" y2=\"{}\"",
               svg_coordinates[0][0],
               svg_coordinates[0][1],
               svg_coordinates[1][0],
               svg_coordinates[1][1])
            .expect("cannot write svg file, disk full ?");
        writeln!(displayer.svg_file, " stroke-width=\"{}\" stroke=\"{}\" \
               opacity=\"0.5\"/>",
               displayer.stroke_width, color)
            .expect("cannot write svg file, disk full ?");
    }
}
