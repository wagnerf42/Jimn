//! polygon clipping algorithm.
use point::Point;
use utils::coordinates_hash::PointsHash;
use bentley_ottmann::{bentley_ottmann, BentleyOttmannPath, Cuttable, HasX};
mod clip_classifier;
use self::clip_classifier::classify_clip_paths;
use overlap::cut_overlaps;

/// Path with tag indicating if its in the clipper.
#[derive(Clone, Copy)]
pub struct ClippingPath<P: BentleyOttmannPath> {
    path: P,
    clipping: bool, // are we clipper or clipped ?
}

impl<P: BentleyOttmannPath> AsRef<P> for ClippingPath<P> {
    fn as_ref(&self) -> &P {
        &self.path
    }
}

impl<P: Copy + BentleyOttmannPath + Cuttable> Cuttable for ClippingPath<P> {
    fn cut<'a, I: 'a + IntoIterator<Item = &'a Point>>(&self, points: I) -> Vec<ClippingPath<P>> {
        self.path
            .cut(points)
            .iter()
            .map(|s| {
                ClippingPath {
                    path: *s,
                    clipping: self.clipping,
                }
            })
            .collect()
    }
    fn new_from(&self, p1: &Point, p2: &Point) -> Self {
        ClippingPath {
            path: self.path.new_from(p1, p2),
            clipping: self.clipping,
        }
    }
}

/// Clip *clipped* paths inside *clipper* paths.
/// Return remaining intersected paths.
/// pre-condition: all endpoints are already hashed in the rounder.
pub fn clip<
    K: HasX + Ord + Copy,
    P: Copy + BentleyOttmannPath<BentleyOttmannKey = K> + Cuttable,
>(
    clipper: &[P],
    clipped: &[P],
    rounder: &mut PointsHash,
) -> Vec<P> {
    let paths: Vec<_> = clipper
        .iter()
        .filter(|p| !p.is_horizontal())
        .map(|p| {
            ClippingPath {
                path: *p,
                clipping: true,
            }
        })
        .chain(clipped.iter().map(|p| {
            ClippingPath {
                path: *p,
                clipping: false,
            }
        }))
        .collect();
    let no_overlap_paths = cut_overlaps(&paths);
    let small_paths = bentley_ottmann(&no_overlap_paths, rounder);
    classify_clip_paths(&small_paths)
}
