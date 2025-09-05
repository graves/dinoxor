#![feature(test)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(not(target_arch = "aarch64"))]
compile_error!("This crate only supports target_arch = \"aarch64\".");

#[cfg_attr(docsrs, doc(cfg(target_arch = "aarch64")))]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod chacha20;
pub mod dinoxor;
#[cfg(test)]
mod tests;
