extern crate jimn;
extern crate rand;
use jimn::{Point, Segment};

fn random_segment() -> Segment {
    Segment::new(
        Point::new(rand::random(), rand::random()),
        Point::new(rand::random(), rand::random()),
    )
}

extern "C" {
    fn nextafter(x: f64, y: f64) -> f64;
}

trait Aftering<'a, T: 'a> {
    fn next_after(&self, towards: T) -> Self;
}

impl<'a> Aftering<'a, f64> for f64 {
    fn next_after(&self, towards: f64) -> f64 {
        unsafe { nextafter(*self, towards) }
    }
}

impl<'a> Aftering<'a, &'a f64> for f64 {
    fn next_after(&self, towards: &'a f64) -> f64 {
        unsafe { nextafter(*self, *towards) }
    }
}

fn main() {
    loop {
        let (s1, s2) = (random_segment(), random_segment());
        let i = s1.intersection_with(&s2);
        if let Some(p) = i {
            let x1 = s1.horizontal_line_intersection(p.y);
            let x2 = s2.horizontal_line_intersection(p.y);
            if x1 != x2 {
                println!(
                    "s1: {:?}, s2: {:?}, i:{:?}, x1:{}, x2:{}",
                    s1, s2, p, x1, x2
                );
                let mut inc_y = p.y;
                let mut dec_y = p.y;
                for _ in 1..10 {
                    inc_y = inc_y.next_after(inc_y + 1.0);
                    let x1 = s1.horizontal_line_intersection(inc_y);
                    let x2 = s2.horizontal_line_intersection(inc_y);
                    if (x1 == x2) {
                        println!("FIXED");
                        break;
                    } else {
                        println!("{} {}", x1, x2);
                    }
                    dec_y = dec_y.next_after(dec_y - 1.0);
                    let x1 = s1.horizontal_line_intersection(dec_y);
                    let x2 = s2.horizontal_line_intersection(dec_y);
                    if (x1 == x2) {
                        println!("FIXED");
                        break;
                    } else {
                        println!("{} {}", x1, x2);
                    }
                }
                break;
            }
        }
    }
}
