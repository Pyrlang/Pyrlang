use cpython::{Python, PyString, PyObject, PythonObject, ObjectProtocol};
use byte::BytesExt;
use byte::ctx::Str;
use compress::zlib;
use std::io::{Read, BufReader};

use super::consts::*;
use super::errors::CodecError;


pub struct Decoder<'a> {
  py: Python<'a>,
  atom_obj: Option<PyObject>,
}

#[derive(Eq, PartialEq)]
pub enum AtomRepresentation {
  TermAtom,
  Bytes,
  String,
}


impl <'a> Decoder<'a> {
  pub fn new(py: Python, aopt: AtomRepresentation) -> Decoder {
    let atom_m = py.import("Term.atom").unwrap();
    let atom_obj = if aopt == AtomRepresentation::TermAtom {
      Some(atom_m.get(py, "Atom").unwrap())
    } else {
      None
    };
    Decoder {
      py,
      atom_obj
    }
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
      d.read_to_end(&mut decompressed).unwrap();
      if decompressed.len() != decomp_size as usize {
        return Err(CodecError::CompressedSizeMismatch)
      }
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


  fn parse_atom(&self, offset: &mut usize,
                in_bytes: &[u8]) -> Result<PyObject, CodecError>
  {
    //let remaining = in_bytes.len() - offset;
    let sz = in_bytes.read_with::<u16>(offset, byte::BE)?;
    let txt = in_bytes.read_with::<&str>(offset, Str::Len(sz as usize))?;

    //let result = PyString::new(self.py, txt);
    let result = self.create_atom(txt)?;
    Ok(result.into_object())
  }


  fn create_atom(&self, txt: &str) -> Result<PyObject, CodecError> {
    let py_txt = PyString::new(self.py, txt);
    match &self.atom_obj {
      None => {
        // Return as a string
        return Ok(py_txt.into_object())
      },
      Some(atom_obj) => {
        // Construct Atom object (Note: performance cost)
        Ok(atom_obj.call(self.py, (py_txt,), None)?)
      },
    } // match
  }
}
