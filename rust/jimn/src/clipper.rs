//! polygon clipping algorithm.
use std::collections::HashSet;
use point::Point;
use segment::Segment;
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::{bentley_ottmann, cut_segments, Cuttable};

#[derive(Clone)]
struct ClippingSegment {
    segment: Segment,
    clipping: bool, // are we clipper or clipped ?
}

impl AsRef<Segment> for ClippingSegment {
    fn as_ref(&self) -> &Segment {
        &self.segment
    }
}

impl Cuttable for ClippingSegment {
    fn cut(&self, points: &HashSet<Point>) -> Vec<ClippingSegment> {
        unimplemented!()
    }
}

/// Clip *clipped* segments inside *clipper* polygon's segments.
/// Return remaining intersected segments.
/// pre-condition: all endpoints are already hashed in the rounder.
pub fn clip(clipper: Vec<Segment>,
            clipped: Vec<Segment>,
            rounder: &mut PointsHash)
            -> Vec<Segment> {
    let segments: Vec<_> = clipper.iter()
        .map(|s| {
                 ClippingSegment {
                     segment: *s,
                     clipping: true,
                 }
             })
        .chain(clipped.iter().map(|s| {
                                      ClippingSegment {
                                          segment: *s,
                                          clipping: false,
                                      }
                                  }))
        .collect();
    let intersections = bentley_ottmann(&segments, rounder);
    let small_segments = cut_segments(&segments, &intersections);
    unimplemented!()
}
