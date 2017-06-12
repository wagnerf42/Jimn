extern crate rand;
use rand::random;

extern crate dyntreap;
use dyntreap::CTreap;

#[test]
fn count() {
    for _ in 0..1000 {
        let elements: Vec<_> = (0..100).into_iter().map(|_| random::<i32>() % 10).collect();
        let mut t = CTreap::new();
        for x in &elements {
            t.insert(*x);
        }
        let mut bounds = vec![Some(random::<i32>() % 10), Some(random::<i32>() % 10)];
        bounds.sort();
        let lower_bound = bounds[0].unwrap();
        let upper_bound = bounds[1].unwrap();

        let iterator_count = t.ordered_nodes(1)
            .lower_bound(lower_bound)
            .upper_bound(upper_bound)
            .len();
        let reversed_iterator_count = t.ordered_nodes(0)
            .lower_bound(lower_bound)
            .upper_bound(upper_bound)
            .len();
        let manual_count = elements
            .iter()
            .filter(|&e| *e > lower_bound && *e < upper_bound)
            .count();
        assert_eq!(iterator_count, manual_count);
        assert_eq!(reversed_iterator_count, manual_count);
    }
}
