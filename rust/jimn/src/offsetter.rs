//! offsetting related functions
use {Arc, ElementaryPath};
use polygon::Polygon;
use holed_polygon::HoledPolygon;
use ordered_float::NotNaN;
use quadrant::{Quadrant, Shape};
use tycat::display;
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::{bentley_ottmann, cut_paths};


/// Add to given vector all paths obtained when taking inner parallel segments in a polygon
/// (displaced by radius) and looping around endpoints.
pub fn inner_paths<T: Into<NotNaN<f64>>>(
    polygon: &Polygon,
    radius: T,
    paths: &mut Vec<ElementaryPath>,
    rounder: &mut PointsHash,
) {
    let radius = radius.into();
    let mut segments = polygon.segments();
    let first_segment = segments.next().unwrap();
    let first_inner_segment =
        ElementaryPath::parallel_segment(&first_segment, radius, true, rounder);
    let start_point = *first_inner_segment.start();
    let start_center = first_segment.start;
    let mut previous_point = *first_inner_segment.end();
    paths.push(first_inner_segment);
    for segment in segments {
        let inner_segment = ElementaryPath::parallel_segment(&segment, radius, true, rounder);
        let arc = Arc::new(
            previous_point,
            *inner_segment.start(),
            segment.start,
            radius,
        );
        let sub_arcs = arc.split_for_unique_y(rounder);
        if let Some((a1, a2)) = sub_arcs {
            paths.push(ElementaryPath::Arc(a1));
            paths.push(ElementaryPath::Arc(a2));
        } else {
            paths.push(ElementaryPath::Arc(arc));
        }

        previous_point = *inner_segment.end();
        paths.push(inner_segment);
    }
    //add last arc
    let last_point = *paths.last().unwrap().end();
    paths.push(ElementaryPath::Arc(
        Arc::new(last_point, start_point, start_center, radius),
    ));
}

/// Offset given `HoledPolygon` at given distance.
/// Return a vector of `HoledPocket`.
pub fn offset_holed_polygon<T: Into<NotNaN<f64>>>(
    holed_polygon: &HoledPolygon,
    radius: T,
    rounder: &mut PointsHash,
) {
    let mut raw_paths = Vec::new();
    let radius = radius.into();
    for polygon in holed_polygon.polygons() {
        inner_paths(polygon, radius, &mut raw_paths, rounder);
    }
    display!(holed_polygon, raw_paths);
    let intersections = bentley_ottmann(&raw_paths, rounder);
    let small_paths = cut_paths(&raw_paths, &intersections);
    display!(holed_polygon, small_paths);
    unimplemented!()
}
