use std::{iter::Map, ops::Add};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Span::new(value.start, value.end)
    }
}

impl Add for Span {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Span::new(self.start, rhs.end)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self(val, span)
    }

    pub fn val(self) -> T {
        self.0
    }

    pub fn span(&self) -> Span {
        self.1
    }

    pub fn extend(self, sp: Span) -> Self {
        Spanned::new(self.0, self.1 + sp)
    }

    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: Fn(T) -> U,
    {
        Spanned::new(f(self.0), self.1)
    }
}
