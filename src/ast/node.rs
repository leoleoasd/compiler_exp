use std::ops::Range;

pub trait Node {
    fn get_location() -> Range<usize>;
}
