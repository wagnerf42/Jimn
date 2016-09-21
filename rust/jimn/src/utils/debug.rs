//! Debugging related utilities.
//! Provides `module_debug!` macro.

/// Jimn makes use of the **$JIMN_DEBUG** environment variable
/// to select code for debugging.
/// You can export a colon separated list of modules for which you
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
            if cfg!(debug_assertions) {
                if let Ok(debugged) = std::env::var("JIMN_DEBUG") {
                    if debugged.split(':')
                        .any(|s| &*s == module_path!()) {
                            $b
                        }
                }
            }
        }
    }
}
