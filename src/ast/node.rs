use dyn_clone::DynClone;
use std::any::Any;
use std::fmt::Debug;
use std::ops::Range;

pub trait Node: DynClone + Debug + Any {
    fn get_location(&self) -> &Range<usize>;
}

dyn_clone::clone_trait_object!(Node);
