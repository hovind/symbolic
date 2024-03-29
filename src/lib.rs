#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        use X::*;

        #[derive(Copy, Clone, Debug, PartialEq)]
        enum X {
            X1,
            X2,
        }
        let x = Expr::cnst(9.0f64) * Expr::var(X1) * Expr::var(X1);
        println!("{}", x.diff(X1));
        println!("{}", x.value(|x| match x {
            X1 => 10.0f64,
            X2 => 4.0f64,
        }))
    }
}


extern crate num_traits as num;

use num::identities::{One, Zero};
use num::float::Float;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Display};
use std::ops::{Add, Mul, Neg};
use std::rc::{Rc};

#[derive(Clone, Debug, PartialEq)]
enum Expr<T, V>
where
{
    Const(T),
    Var(V),
    Id(Rc<Expr<T, V>>),
    Neg(Rc<Expr<T, V>>),
    Sum(Rc<Expr<T, V>>, Rc<Expr<T, V>>),
    Prod(Rc<Expr<T, V>>, Rc<Expr<T, V>>),
}

impl<T, V> PartialOrd for Expr<T, V> where
    T: PartialOrd,
    V: PartialEq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Expr::Const(a), Expr::Const(b)) => a.partial_cmp(b),
            (a, Expr::Id(b)) | (Expr::Id(b), a) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl<T, V> Display for Expr<T, V> where
    T: Display,
    V: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Const(a) => write!(f, "{}", a),
            Expr::Var(v) =>  write!(f, "{:?}", v),
            Expr::Id(expr) => write!(f, "{}", &expr),
            Expr::Neg(expr) => write!(f, "-{}", &expr),
            Expr::Sum(lhs, rhs) => write!(f, "{} + {}", &lhs, &rhs),
            Expr::Prod(lhs, rhs) => write!(f, "{} * {}", &lhs, &rhs),
        }

    }
}

impl<T, V> One for Expr<T, V> where
    T: Copy + Mul<T, Output = T> + One + PartialEq + Zero,
{
    fn one() -> Self {
        Expr::Const(T::one())
    }
    fn is_one(self : &Self) -> bool {
        match self {
            Expr::Const(a) => a.is_one(),
            _ => false,
        }
    }
}

impl<T, V> Zero for Expr<T, V> where
    T: Add<T, Output = T> + Copy + Zero,
{
    fn zero() -> Self {
        Expr::Const(T::zero())
    }
    fn is_zero(self : &Self) -> bool {
        match self {
            Expr::Const(a) => a.is_zero(),
            _ => false,
        }
    }
}

impl<T, V> Neg for Expr<T, V> where
    T: Copy + Neg<Output = T>,
{
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Expr::Const(a) => Expr::Const(a.neg()),
            Expr::Neg(a) => Expr::Id(Rc::clone(&a)),
            a => Expr::Neg(Rc::new(a)),
            }
    }

}

impl<T, V> Add for Expr<T, V> where
    T: Add<T, Output = T> + Copy + Zero,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expr::Const(a), Expr::Const(b)) => Expr::Const(a + b),
            (Expr::Const(a), b) | (b, Expr::Const(a)) if a.is_zero() => b,
            (a, Expr::Id(b)) | (Expr::Id(b), a) => a + b,
            (a, b) => Expr::Sum(Rc::new(a), Rc::new(b)),
            }
    }

}

impl<T, V> Add<Rc<Self>> for Expr<T, V> where
    T: Add<T, Output = T> + Copy + Zero,
{
    type Output = Self;

    fn add(self, rhs: Rc<Self>) -> Self::Output {
        match (self, &*rhs) {
            (Expr::Const(a), &Expr::Const(b)) => Expr::Const(a + b),
            (Expr::Const(a), _) if a.is_zero() => Expr::Id(rhs),
            (a, &Expr::Const(b)) if b.is_zero() => a,
            (a, _) => Expr::Sum(Rc::new(a), rhs),
        }
    }
}

impl<T, V> Mul for Expr<T, V> where
    T: Copy + Mul<T, Output = T> + One + PartialEq + Zero,
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Expr::Const(a), Expr::Const(b)) => Expr::Const(a * b),
            (Expr::Const(a), _) | (_, Expr::Const(a)) if a.is_zero() => Expr::zero(),
            (Expr::Const(a), b) | (b, Expr::Const(a)) if a.is_one() => Expr::Id(Rc::new(b)),
            (a, Expr::Id(b)) | (Expr::Id(b), a) => a * b,
            (a, b) => Expr::Prod(Rc::new(a), Rc::new(b)),
        }
    }

}

impl<T, V> Mul<Rc<Self>> for Expr<T, V> where
    T: Copy + Mul<T, Output = T> + One + PartialEq + Zero,
{
    type Output = Self;

    fn mul(self, rhs: Rc<Self>) -> Self {
        match (self, &*rhs) {
            (Expr::Const(a), &Expr::Const(b)) => Expr::Const(a * b),
            (Expr::Const(a), _) | (_, &Expr::Const(a)) if a.is_zero() => Expr::zero(),
            (Expr::Const(a), _) if a.is_one() => Expr::Id(rhs),
            (a, &Expr::Const(b)) if b.is_one() => a,
            (a, _) => Expr::Prod(Rc::new(a), rhs),
        }
    }

}

impl<T, V> Expr<T, V> where
    T: Add<T, Output = T> + Copy + Mul<T, Output = T> + Neg<Output = T> + One + PartialEq + Zero,
    V: Copy + PartialEq,
{
    pub fn var(v: V) -> Expr<T, V> {
        Expr::Var(v)
    }
    pub fn cnst(t: T) -> Expr<T, V> {
        Expr::Const(t)
    }
    pub fn diff(self : &Self, del : V) -> Self {
        match self {
            &Expr::Const(_) => Expr::zero(),
            &Expr::Var(v) if del == v => Expr::one(),
            &Expr::Var(_) => Expr::zero(),
            Expr::Id(expr) => expr.diff(del),
            Expr::Neg(expr) => -expr.diff(del),
            Expr::Sum(lhs, rhs) => lhs.diff(del) + rhs.diff(del),
            Expr::Prod(lhs, rhs) => lhs.diff(del) * Rc::clone(rhs) + rhs.diff(del) * Rc::clone(lhs),
        }
    }
    pub fn value<F>(self : &Self, f : F) -> T where
        F: Copy + Fn(V) -> T,
    {
        match self {
            &Expr::Const(a) => a,
            &Expr::Var(v) => f(v),
            Expr::Id(expr) => expr.value(f),
            Expr::Neg(expr) => -expr.value(f),
            Expr::Sum(lhs, rhs) => lhs.value(f) + rhs.value(f),
            Expr::Prod(lhs, rhs) => lhs.value(f) * rhs.value(f),
        }
    }
}

impl<T, V> num::cast::ToPrimitive for Expr<T, V>
where
    T: num::cast::ToPrimitive,
{
    fn to_i64(&self) -> Option<i64> {
        match self {
            Expr::Const(a) => a.to_i64(),
            _ => None,
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            Expr::Const(a) => a.to_u64(),
            _ => None,
        }
    }

    fn to_isize(&self) -> Option<isize> {
        match self {
            Expr::Const(a) => a.to_isize(),
            _ => None,
        }
    }

    fn to_i8(&self) -> Option<i8> {
        match self {
            Expr::Const(a) => a.to_i8(),
            _ => None,
        }
    }

    fn to_i16(&self) -> Option<i16> {
        match self {
            Expr::Const(a) => a.to_i16(),
            _ => None,
        }
    }

    fn to_i32(&self) -> Option<i32> {
        match self {
            Expr::Const(a) => a.to_i32(),
            _ => None,
        }
    }

    fn to_i128(&self) -> Option<i128> {
        match self {
            Expr::Const(a) => a.to_i128(),
            _ => None,
        }
    }

    fn to_usize(&self) -> Option<usize> {
        match self {
            Expr::Const(a) => a.to_usize(),
            _ => None,
        }
    }

    fn to_u8(&self) -> Option<u8> {
        match self {
            Expr::Const(a) => a.to_u8(),
            _ => None,
        }
    }

    fn to_u16(&self) -> Option<u16> {
        match self {
            Expr::Const(a) => a.to_u16(),
            _ => None,
        }
    }

    fn to_u32(&self) -> Option<u32> {
        match self {
            Expr::Const(a) => a.to_u32(),
            _ => None,
        }
    }

    fn to_u128(&self) -> Option<u128> {
        match self {
            Expr::Const(a) => a.to_u128(),
            _ => None,
        }
    }

    fn to_f32(&self) -> Option<f32> {
        match self {
            Expr::Const(a) => a.to_f32(),
            _ => None,
        }
    }

    fn to_f64(&self) -> Option<f64> {
        match self {
            Expr::Const(a) => a.to_f64(),
            _ => None,
        }
    }
}

impl<T, V> num::cast::NumCast for Expr<T, V>
where
    T: num::cast::NumCast,
{
    fn from<S: num::cast::ToPrimitive>(n: S) -> Option<Self> {
        T::from(n).map(Expr::Const)
    }
}

impl<T, V, E> num::Num for Expr<T, V> where
    T: num::Num<FromStrRadixErr = E>
{
    type FromStrRadixErr = E;

    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        T::from_str_radix(str, radix).map(Expr::Const)
    }
}
