use cpython::{exc, PyErr, PyTuple};
use cpython::{Python, PythonObject};
use super::PyCodecError;

#[derive(Debug, Fail)]
pub enum CodecError {
  #[fail(display="ETF version 131 is expected")]
  UnsupportedETFVersion,
  #[fail(display="Input is empty")]
  EmptyInput,
  #[fail(display="Compressed size does not match decompressed")]
  CompressedSizeMismatch,
  #[fail(display="Read failed: {}", txt)]
  ReadError { txt: String },
  #[fail(display="PyO3 Python error: {}", txt)]
  PythonError { txt: String },
  #[fail(display="Unrecognized term tag byte: {}", b)]
  UnknownTermTagByte { b: u8 },
}


impl std::convert::From<PyErr> for CodecError {
  fn from(err: PyErr) -> CodecError {
    CodecError::PythonError { txt: format!("{:?}", err) }
  }
}


impl std::convert::From<byte::Error> for CodecError {
  fn from(err: byte::Error) -> CodecError {
    CodecError::ReadError { txt: format!("{:?}", err) }
  }
}


impl std::convert::From<CodecError> for PyErr {
  fn from(err: CodecError) -> PyErr {
    let gil = Python::acquire_gil();
    let py = gil.python();
    let ty = py.get_type::<PyCodecError>();

    let noargs = PyTuple::empty(py);
    let err_val = ty.call(py, noargs, None).unwrap();
    let tyo = ty.into_object();
    PyErr {
      ptype: tyo,
      pvalue: Some(err_val),
      ptraceback: None,
    }
  }
}
