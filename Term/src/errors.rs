use cpython::*;

use super::PyCodecError;

#[derive(Debug, Fail)]
pub enum CodecError {
  #[fail(display="Feature is not implemented yet")]
  NotImpl,
  #[fail(display="ETF version 131 is expected")]
  UnsupportedETFVersion,
  #[fail(display="Input is empty")]
  EmptyInput,
  #[fail(display="Compressed size does not match decompressed")]
  CompressedSizeMismatch,
  #[fail(display="Read failed: {}", txt)]
  ReadError { txt: String },
  #[fail(display="Python error: {}", txt)]
  PythonError { txt: String },
  #[fail(display="Unrecognized term tag byte: {}", b)]
  UnknownTermTagByte { b: u8 },
  #[fail(display="Bad options passed: {}", txt)]
  BadOptions { txt: String },
}

pub type CodecResult<T> = Result<T, CodecError>;


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
  /// Somehow this works. Create a PyErr struct without traceback, containing
  /// a PyCodecError created from Rust CodecError with string explanation.
  fn from(err: CodecError) -> PyErr {
    let gil = Python::acquire_gil();
    let py = gil.python();
    let ty = py.get_type::<PyCodecError>();

    // CodecErrors are formatted using #[fail...] attribute format string
    let err_str = format!("{}", err);
    let py_str = PyString::new(py, &err_str);
    let noargs = PyTuple::new(py, &vec![py_str.into_object()]);
    let err_val = ty.call(py, noargs, None).unwrap();

    let tyo = ty.into_object();
    PyErr {
      ptype: tyo,
      pvalue: Some(err_val),
      ptraceback: None,
    }
  }
}


/// Repacks CodecResult<T> into PyResult<T>
pub fn pyResult_from<T>(r: Result<T, CodecError>) -> Result<T, PyErr> {
  match r {
    Ok(x) => Ok(x),
    Err(e) => Err(PyErr::from(e)),
  }
}