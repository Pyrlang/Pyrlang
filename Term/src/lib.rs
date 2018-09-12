#[macro_use] extern crate cpython;
extern crate compress;
extern crate byte;

#[macro_use]
extern crate failure;

use cpython::{PyResult, Python, PyObject, PyBytes};

use self::decoder::Decoder;

mod consts;
mod decoder;
mod encoder;
mod errors;
mod wrappers;


/// Strips 131 byte header and unpacks if the data was compressed.
fn binary_to_term(py: Python, b: PyBytes) -> PyResult<PyObject> {
  let mut dec_state = Decoder::new(py);
  Ok(dec_state.binary_to_term(b.data(py))?)
}


// add bindings to the generated python module
// N.B: names: "librust2py" must be the name of the `.so` or `.pyd` file
py_module_initializer!(term_codec, initterm_codec, PyInit_term_codec,
|py, m| {
    m.add(py, "__doc__", "Erlang Term Format encoding and decoding.");
    m.add(py, "binary_to_term", py_fn!(py, binary_to_term(b: PyBytes)));
    Ok(())
});


py_exception!(term_codec, PyCodecError);
