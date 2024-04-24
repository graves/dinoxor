#![feature(test)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod thechinesegovernment;
#[cfg(test)]
mod tests;