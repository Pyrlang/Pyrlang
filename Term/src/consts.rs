#![allow(unused)]

use pyo3::{exc, PyErr};

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


impl std::convert::From<pyo3::PyErr> for CodecError {
  fn from(err: pyo3::PyErr) -> CodecError {
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
    exc::ValueError.into()
  }
}


pub const ETF_VERSION_TAG: u8 = 131;

pub const TAG_NEW_FLOAT_EXT: u8 = 70;
pub const TAG_BIT_BINARY_EXT: u8 = 77;
pub const TAG_COMPRESSED: u8 = 80;
pub const TAG_SMALL_INT: u8 = 97;
pub const TAG_INT: u8 = 98;
pub const TAG_FLOAT_EXT: u8 = 99;
pub const TAG_ATOM_EXT: u8 = 100;
pub const TAG_PID_EXT: u8 = 103;
pub const TAG_SMALL_TUPLE_EXT: u8 = 104;
pub const TAG_LARGE_TUPLE_EXT: u8 = 105;
pub const TAG_NIL_EXT: u8 = 106;
pub const TAG_STRING_EXT: u8 = 107;
pub const TAG_LIST_EXT: u8 = 108;
pub const TAG_BINARY_EXT: u8 = 109;
pub const TAG_SMALL_BIG_EXT: u8 = 110;
pub const TAG_LARGE_BIG_EXT: u8 = 111;
pub const TAG_NEW_FUN_EXT: u8 = 112;
pub const TAG_NEW_REF_EXT: u8 = 114;
pub const TAG_SMALL_ATOM_EXT: u8 = 115;
pub const TAG_MAP_EXT: u8 = 116;
pub const TAG_ATOM_UTF8_EXT: u8 = 118;
pub const TAG_SMALL_ATOM_UTF8_EXT: u8 = 119;
