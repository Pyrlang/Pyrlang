#![feature(specialization)]

#[macro_use]
extern crate pyo3;

mod defs;
mod encoder;
mod decoder;

use pyo3::prelude::*;
use pyo3::{Python, PyErr, PyBytes, exc};

//use std::convert::From;
use self::defs::*;

//impl std::convert::From<CodecError> for PyErr {
//  fn from(err: CodecError) -> PyErr {
//    exc::ValueError.into()
//  }
//}


#[pyfunction]
/// Strips 131 byte header and unpacks if the data was compressed.
fn binary_to_term(in_bytes: &PyBytes) -> PyResult<String> {
  let data = in_bytes.data();
  if data[0] != ETF_VERSION_TAG {
    return Err(exc::ValueError::new("Unsupported external term version"))
  }
  else if in_bytes.empty() {
    return Err(exc::ValueError::new("Input binary is empty"))
  }

  Ok("hello".to_string())
}

/// This module is a python moudle implemented in Rust.
#[pymodinit]
fn term_codec(py: Python, m: &PyModule) -> PyResult<()> {
  m.add_function(wrap_function!(binary_to_term))?;

  Ok(())
}
