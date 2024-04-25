#![feature(test)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod dinoxor;
pub mod chacha20;
#[cfg(test)]
mod tests;