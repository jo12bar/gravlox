use std::any::Any;
use std::fmt;

mod sealed {
    use super::*;

    pub trait LiteralExt {
        /// Clone literal into a new box.
        fn clone_box(&self) -> Box<dyn Literal>;

        /// Cast literal into a &dyn Any.
        fn as_any(&self) -> &dyn Any;
    }

    impl<T> LiteralExt for T
    where
        T: 'static + Literal + Clone,
    {
        fn clone_box(&self) -> Box<dyn Literal> {
            Box::new(self.clone())
        }

        fn as_any(&self) -> &dyn Any {
            println!("-- convert type {:?} to Any", self.type_id());
            self
        }
    }
}

pub trait Literal: fmt::Display + fmt::Debug + sealed::LiteralExt {}

impl<T> Literal for Box<T> where T: Literal + Clone + 'static {}

impl Clone for Box<dyn Literal> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl Literal for &'static str {}
impl Literal for String {}

impl Literal for f64 {}
impl Literal for bool {}
