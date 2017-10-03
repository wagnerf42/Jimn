//!this hashing code is taken from ordered_float crate
use std::hash::{Hash, Hasher};
use num_traits::float::Float;
use std::mem;

/// hash a float.
#[inline]
pub fn hash_float<H: Hasher>(f: &f64, state: &mut H) {
    raw_double_bits(f).hash(state);
}

// canonical raw bit patterns (for hashing)
const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

// masks for the parts of the IEEE 754 float
const SIGN_MASK: u64 = 0x8000000000000000u64;
const EXP_MASK: u64 = 0x7ff0000000000000u64;
const MAN_MASK: u64 = 0x000fffffffffffffu64;

#[inline]
fn raw_double_bits(f: &f64) -> u64 {
    if f.is_nan() {
        return CANONICAL_NAN_BITS;
    }

    let (man, exp, sign) = f.integer_decode();
    if man == 0 {
        return CANONICAL_ZERO_BITS;
    }

    let exp_u64 = unsafe { mem::transmute::<i16, u16>(exp) } as u64;
    let sign_u64 = if sign > 0 { 1u64 } else { 0u64 };
    (man & MAN_MASK) | ((exp_u64 << 52) & EXP_MASK) | ((sign_u64 << 63) & SIGN_MASK)
}
