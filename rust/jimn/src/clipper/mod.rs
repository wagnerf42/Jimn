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
            .map(|s| ClippingPath {
                path: *s,
                clipping: self.clipping,
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
/// Return all paths from clipper and smaller non overlapping paths from clipped.
/// pre-condition: all endpoints are already hashed in the rounder.
pub fn clip<
    'a,
    K: HasX + Ord + Copy,
    P: 'a + Copy + BentleyOttmannPath<BentleyOttmannKey = K> + Cuttable,
    I: IntoIterator<Item = &'a P>,
    J: IntoIterator<Item = &'a P>,
>(
    clipper: I,
    clipped: J,
    rounder: &mut PointsHash,
) -> (Vec<P>, Vec<P>) {
    // it is very important to add clipped paths first since they stay in case of overlap
    let paths: Vec<_> = clipper
        .into_iter()
        .map(|p| ClippingPath {
            path: *p,
            clipping: true,
        })
        .chain(clipped.into_iter().map(|p| ClippingPath {
            path: *p,
            clipping: false,
        }))
        .collect();
    module_debug!({
        println!("starting clipper on:");
        display!(
            unicolor!(paths.iter().filter(|p| p.clipping).map(|p| p.as_ref())),
            unicolor!(paths.iter().filter(|p| !p.clipping).map(|p| p.as_ref()))
        );
    });
    let no_overlap_paths = cut_overlaps(&paths);
    module_debug!({
        println!("after discarding overlapping inside:");
        display!(
            unicolor!(
                no_overlap_paths
                    .iter()
                    .filter(|p| p.clipping)
                    .map(|p| p.as_ref())
            ),
            unicolor!(
                no_overlap_paths
                    .iter()
                    .filter(|p| !p.clipping)
                    .map(|p| p.as_ref())
            )
        );
    });
    let small_paths = bentley_ottmann(&no_overlap_paths, rounder);
    module_debug!({
        println!("after cutting into small paths:");
        display!(
            unicolor!(
                small_paths
                    .iter()
                    .filter(|p| p.clipping)
                    .map(|p| p.as_ref())
            ),
            unicolor!(
                small_paths
                    .iter()
                    .filter(|p| !p.clipping)
                    .map(|p| p.as_ref())
            )
        );
    });
    classify_clip_paths(&small_paths)
}
