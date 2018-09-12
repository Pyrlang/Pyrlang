use cpython::{Python, PyString, PyObject, ToPyObject, PythonObject};
use byte::BytesExt;
use byte::ctx::Str;
use compress::zlib;
//use pyo3::prelude::*;
use std::io::{Read, BufReader};

//use super::wrappers::{Atom};
use super::consts::*;
use super::errors::CodecError;


pub struct Decoder<'a> {
  py: Python<'a>,
}


impl <'a> Decoder<'a> {
  pub fn new(py: Python) -> Decoder {
    Decoder { py }
  }


  /// Strips 131 byte header and unpacks if the data was compressed.
  pub fn binary_to_term(&mut self, in_bytes: &[u8]) -> Result<PyObject, CodecError> {
    let offset = &mut 0;

    let pre_tag = in_bytes.read_with::<u8>(offset, byte::BE)?;
    if pre_tag != ETF_VERSION_TAG {
      return Err(CodecError::UnsupportedETFVersion)
    } else if in_bytes.is_empty() {
      return Err(CodecError::EmptyInput)
    }

    let tag = in_bytes.read_with::<u8>(offset, byte::BE)?;
    if tag == TAG_COMPRESSED {
      let decomp_size = in_bytes.read_with::<u32>(offset, byte::BE)?;

      let tail1 = &in_bytes[*offset..in_bytes.len()];
      let mut decompressed = Vec::<u8>::new();
      let mut d = zlib::Decoder::new(BufReader::new(tail1));
      d.read_to_end(&mut decompressed);
      if decompressed.len() != decomp_size as usize {
        return Err(CodecError::CompressedSizeMismatch)
      }
//    if len(decomp) != decomp_size:
//    # Data corruption?
//    raise ETFDecodeException("Compressed size mismatch with actual")
      return self.binary_to_term_2(decompressed.as_ref())
    }

    // Second byte was not consumed, so restart parsing from the second byte
    let tail2 = &in_bytes[1..in_bytes.len()];
    return self.binary_to_term_2(tail2)
  }


  /// Decodes binary External Term Format (ETF) into a Python structure.
  pub fn binary_to_term_2(&self, in_bytes: &[u8]) -> Result<PyObject, CodecError> {
    //create_atom("hello")

    let offset = &mut 0;
    let tag = in_bytes.read_with::<u8>(offset, byte::BE)?;
    match tag {
      TAG_ATOM_EXT => self.parse_atom(offset, in_bytes),
      TAG_ATOM_UTF8_EXT => self.parse_atom(offset, in_bytes),
      _ => Err(CodecError::UnknownTermTagByte { b: tag }),
    }
  }


  fn parse_atom(&self,
                offset: &mut usize,
                in_bytes: &[u8]) -> Result<PyObject, CodecError>
  {
    //let remaining = in_bytes.len() - offset;
    let sz = in_bytes.read_with::<u16>(offset, byte::BE)?;
    let txt = in_bytes.read_with::<&str>(offset, Str::Len(sz as usize))?;

//    let gil = Python::acquire_gil();
//    let py = gil.python();
    let result = PyString::new(self.py, txt);
    Ok(result.into_object())
  }
//
//
//  fn parse_atom_utf8(&self,
//                     offset: &mut usize,
//                     in_bytes: &[u8]) -> Result<PyObject, CodecError>
//  {
//    //let remaining = in_bytes.len() - offset;
//    let sz = in_bytes.read_with::<u16>(offset, byte::BE)?;
//    let txt = in_bytes.read_with::<&str>(offset, Str::Len(sz as usize))?;
//
//    let gil = Python::acquire_gil();
//    let py = gil.python();
//    let result = PyString::new(py, txt);
//    Ok(result.to_object(py))
//  }
}
