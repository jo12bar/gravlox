use std::fmt;

mod sealed {
    use super::*;

    pub trait LiteralClone {
        /// Clone literal into a new box.
        fn clone_box(&self) -> Box<dyn Literal>;
    }

    impl<T> LiteralClone for T
    where
        T: 'static + Literal + Clone,
    {
        fn clone_box(&self) -> Box<dyn Literal> {
            Box::new(self.clone())
        }
    }
}

pub trait Literal: fmt::Display + fmt::Debug + sealed::LiteralClone {}

impl Clone for Box<dyn Literal> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl Literal for &'static str {}
impl Literal for String {}

impl Literal for f64 {}
impl Literal for bool {}
