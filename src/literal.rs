use std::fmt;

pub trait Literal: fmt::Display + fmt::Debug {}

impl Literal for &str {}
impl Literal for String {}

impl Literal for f64 {}
