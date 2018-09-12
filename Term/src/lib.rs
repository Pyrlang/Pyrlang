#![feature(specialization)]

#[macro_use]
extern crate pyo3;
extern crate compress;
extern crate byte;

#[macro_use]
extern crate failure;

use pyo3::prelude::*;
use pyo3::{Python, PyBytes};
use self::decoder::Decoder;

mod consts;
mod decoder;
mod encoder;
mod wrappers;


#[pyfunction]
/// Strips 131 byte header and unpacks if the data was compressed.
fn binary_to_term(in_bytes_py: &PyBytes) -> PyResult<PyObject> {
  let mut dec_state = Decoder::new();
  Ok(dec_state.binary_to_term(in_bytes_py.data())?)
}


/// This module is a python moudle implemented in Rust.
#[pymodinit]
fn term_codec(py: Python, m: &PyModule) -> PyResult<()> {
  m.add_function(wrap_function!(binary_to_term))?;

  Ok(())
}
