#[macro_use] extern crate cpython;
extern crate compress;
extern crate byte;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate failure;

use cpython::{PyResult, PyErr, Python, PyModule, PyObject, PyBytes, PyDict};

use self::decoder::{Decoder, AtomRepresentation};
use self::errors::{CodecError};

mod consts;
mod decoder;
mod encoder;
mod errors;
mod wrappers;

py_exception!(term_codec, PyCodecError);


/// Strips 131 byte header and unpacks if the data was compressed.
fn binary_to_term(py: Python, b: PyBytes,
                  opts: PyObject) -> PyResult<PyObject> {
  let mut dec_state = Decoder::new(
    py, AtomRepresentation::String,
  );
  Ok(dec_state.binary_to_term(b.data(py))?)
}


fn binary_to_term_2(py: Python, b: PyBytes,
                    opts: PyObject) -> PyResult<PyObject> {
  Err(PyErr::from(CodecError::NotImpl))
}


fn term_to_binary(py: Python, b: PyBytes) -> PyResult<PyObject> {
  Err(PyErr::from(CodecError::NotImpl))
}


fn term_to_binary_2(py: Python, b: PyBytes) -> PyResult<PyObject> {
  Err(PyErr::from(CodecError::NotImpl))
}


// add bindings to the generated python module
// N.B: names: "librust2py" must be the name of the `.so` or `.pyd` file
#[inline]
fn m_init(py: Python, m: &PyModule) -> PyResult<()> {
  m.add(py, "__doc__", "Erlang Term Format encoding and decoding.")?;
  m.add(py, "binary_to_term",
        py_fn!(py, binary_to_term(b: PyBytes, opt: PyObject)))?;
  m.add(py, "binary_to_term_2",
        py_fn!(py, binary_to_term_2(b: PyBytes, opt: PyObject)))?;
  m.add(py, "term_to_binary",
        py_fn!(py, term_to_binary(b: PyBytes)))?;
  m.add(py, "term_to_binary_2",
        py_fn!(py, term_to_binary_2(b: PyBytes)))?;
  Ok(())
}
py_module_initializer!(
  native_codec_impl,
  initnative_codec_impl,
  PyInit_native_codec_impl,
  |py, m| { m_init(py, m) }
  );
