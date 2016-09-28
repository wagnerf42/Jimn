//! We factorize here all common parts in sweeping line algorithms.
use std::collections::BinaryHeap;
use elementary_path::ElementaryPath;
use point::Point;

/// Contains current state of sweeping line algorithms:
///  - all remaining events
///  - currently crossed paths
pub struct SweepingLineState {
    /// clones of all paths given at start (for debugging purposes)
    paths: Vec<Box<ElementaryPath>>,
    // all remaining events in system
    //events: BinaryHeap<(Point, Box<ElementaryPath>)>
    // TODO: crossed paths
}

/// `SweepingLineState` implementation handles iterations on paths for
/// sweeping line algorithms.
/// Any such algorithm just needs to provide callbacks for actions on
/// start and end of paths.
pub trait SweepingLineAlgorithm {
    /// Handler called when a new path is encountered.
    fn add_path(&mut self, state: &mut SweepingLineState);
    /// Handler called when an old path is completed.
    fn remove_path(&mut self, state: &mut SweepingLineState);
}

impl SweepingLineState {
    /// Creates a new state object for running sweeping line algorithms.
    fn new(paths: Vec<Box<ElementaryPath>>) -> SweepingLineState {
        SweepingLineState {
            paths: paths,
            //events: BinaryHeap::new()
        }
    }
    /// Sweeps through all paths, calling the algorithm's callbacks when
    /// starting or ending paths.
    fn run<T: SweepingLineAlgorithm>(&mut self, algorithm: &mut T) {
        println!("TODO run !");
    }
    //add_event: TODO
}


