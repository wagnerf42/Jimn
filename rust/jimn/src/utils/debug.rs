//! Debugging related utilities.
//! Provides `module_debug!` macro.

/// Jimn makes use of the `$JIMN_DEBUG` environment variable
/// to select code for debugging.
/// You can export a spaces separated list of modules for which you
/// want to activate logs (even graphical logs in terminology).
///
/// Using `module_debug!` on some code block will ensure that:
///
/// * in release target, the block vanishes from code ;
/// * in debug target when the module containing the macro call is not debugged, the
/// code is not executed.
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate jimn;
/// # fn main() {
/// module_debug!({
///     println!("this message is only seen if $JIMN_DEBUG contains current module_path!()");
/// })
/// # }
/// ```
#[macro_export]
macro_rules! module_debug {
    ($b:block) => {
        {
            use std;
            if cfg!(debug_assertions) {
                if let Ok(debugged) = std::env::var("JIMN_DEBUG") {
                    if debugged.split(' ')
                        .any(|s| &*s == module_path!()) {
                            $b
                        }
                }
            }
        }
    }
}

use std;
use std::fmt::Debug;
use quadrant::Shape;

/// Allow for debugging without requiring `std::fmt::Debug`
/// taken from
/// `https://www.reddit.com/r/rust/comments/6poulm/tip_print_a_t_without_requiring_t_debug/`
pub trait AsDebug {
    /// convert self to &Debug if we can or panic.
    fn as_debug(&self) -> &Debug;
}

impl<T> AsDebug for T {
    default fn as_debug(&self) -> &Debug {
        panic!("Debug not implemented for {}", unsafe {
            std::intrinsics::type_name::<T>()
        });
    }
}

impl<T: Debug> AsDebug for T {
    fn as_debug(&self) -> &Debug {
        self
    }
}

/// We can convert some objects into `Shape` for easy debugging.
pub trait AsShape {
    /// convert self to &Shape if we can or panic.
    fn as_shape(&self) -> &Shape;
}

impl<T> AsShape for T {
    default fn as_shape(&self) -> &Shape {
        panic!("Shape not implemented for {}", unsafe {
            std::intrinsics::type_name::<T>()
        });
    }
}

impl<T: Shape> AsShape for T {
    fn as_shape(&self) -> &Shape {
        self
    }
}
