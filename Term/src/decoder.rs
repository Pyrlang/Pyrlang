use cpython::*;
use byte::BytesExt;
use byte::ctx::Str;
use compress::zlib;
use std::io::{Read, BufReader};

use super::helpers;
use super::consts::*;
use super::errors::*;


#[derive(Eq, PartialEq)]
enum Encoding {
  Latin1,
  UTF8
}


#[derive(Eq, PartialEq)]
enum AtomRepresentation {
  TermAtom,
  Bytes,
  Str,
}


pub struct Decoder<'a> {
  py: Python<'a>,
  cached_atom_class: Option<PyObject>,
  atom_representation: AtomRepresentation,
}


impl <'a> Decoder<'a> {
  /// Create decoder instance. Parse options.
  pub fn new(py: Python, opts: PyObject) -> CodecResult<Decoder> {
    // If opts is None, make it empty Dict, otherwise take it as PyDict
    let opts1 = helpers::maybe_dict(py, opts);

    // Option: "atom" => "bytes" | "str" | "Atom" (as Atom class, default)
    let aopt_s: String = helpers::get_str_opt(py, opts1, "atom", "Atom")?;
    let aopt = match aopt_s.as_ref() {
      "bytes" => AtomRepresentation::Bytes,
      "str" => AtomRepresentation::Str,
      "Atom" => AtomRepresentation::TermAtom,
      other => {
        let txt = format!(
          "'atom' option is '{}' while expected: bytes, str, Atom", other);
        return Err(CodecError::BadOptions {txt})
      }
    };

    Ok(Decoder {
      py,
      atom_representation: aopt,
      cached_atom_class: None,
    })
  }


  /// Return cached value of atom class used for decoding. Otherwise if not
  /// found - import and cache it locally.
  fn get_atom_class(&mut self) -> PyObject {
    match &self.cached_atom_class {
      Some(ref a) => {
        a.clone_ref(self.py)
      },
      None => {
        let atom_m = self.py.import("Term.atom").unwrap();
        let atom_cls = atom_m.get(self.py, "Atom").unwrap();
        self.cached_atom_class = Some(atom_cls.clone_ref(self.py));
        atom_cls
      },
    }
  }


  /// Strip 131 byte header and uncompress if the data was compressed.
  /// Return: PyTuple(PyObject, Bytes) or CodecError
  pub fn binary_to_term(&mut self, in_bytes: &[u8]) -> CodecResult<PyObject>
  {
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

      let r1 = self.binary_to_term_2(decompressed.as_ref());
      return wrap_decode_result(self.py, r1)
    }

    // Second byte was not consumed, so restart parsing from the second byte
    let tail2 = &in_bytes[1..in_bytes.len()];
    let r2 = self.binary_to_term_2(tail2);
    return wrap_decode_result(self.py, r2)
  }


  /// Decodes binary External Term Format (ETF) into a Python structure.
  /// Returns: (Decoded object, remaining bytes) or CodecError
  pub fn binary_to_term_2<'inp>(&mut self,
                                in_bytes: &'inp [u8]
                                ) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let offset = &mut 0;
    let tag = in_bytes.read_with::<u8>(offset, byte::BE)?;
    match tag {
      TAG_ATOM_EXT =>
        self.parse_atom::<u16>(offset, in_bytes, Encoding::Latin1),
      TAG_ATOM_UTF8_EXT =>
        self.parse_atom::<u16>(offset, in_bytes, Encoding::UTF8),
      TAG_SMALL_ATOM_EXT =>
        self.parse_atom::<u8>(offset, in_bytes, Encoding::Latin1),
      TAG_SMALL_ATOM_UTF8_EXT =>
        self.parse_atom::<u8>(offset, in_bytes, Encoding::UTF8),
      TAG_BINARY_EXT =>
        self.parse_binary(offset, in_bytes),
      _ =>
        Err(CodecError::UnknownTermTagByte { b: tag }),
    }
  }


  /// Parses bytes after Atom tag (100) or Atom Utf8 (118)
  /// Returns: Tuple (string | bytes | Atom object, remaining bytes)
  #[inline]
  fn parse_atom<'inp, T>(
    &mut self, offset: &mut usize, in_bytes: &'inp [u8],
    coding: Encoding,
  ) -> CodecResult<(PyObject, &'inp [u8])>
    where usize: std::convert::From<T>,
          T: byte::TryRead<'inp, byte::ctx::Endian>
  {
    let sz = in_bytes.read_with::<T>(offset, byte::BE)?;
    let txt = in_bytes.read_with::<&str>(offset,
                                         Str::Len(usize::from(sz)))?;

    let result = self.create_atom(txt)?.into_object();
    let remaining = &in_bytes[*offset..in_bytes.len()];
    Ok((result, remaining))
  }


  // TODO: Make 3 functions and store fun pointer
  fn create_atom(&mut self, txt: &str) -> CodecResult<PyObject> {
    match self.atom_representation {
      AtomRepresentation::Bytes => {
        let py_bytes = PyBytes::new(self.py, txt.as_ref());
        Ok(py_bytes.into_object())
      },
      AtomRepresentation::Str => {
        // Return as a string
        let py_txt = PyString::new(self.py, txt);
        Ok(py_txt.into_object())
      },
      AtomRepresentation::TermAtom => {
        // Construct Atom object (Note: performance cost)
        let py_txt = PyString::new(self.py, txt);
        let atom_obj = self.get_atom_class();
        Ok(atom_obj.call(self.py, (py_txt,), None)?)
      },
    } // match
  }


  /// Given input _after_ binary tag, parse remaining bytes
  #[inline]
  fn parse_binary<'inp>(&self, offset: &mut usize,
                        in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])> {
    let sz = in_bytes.read_with::<u32>(offset, byte::BE)? as usize;
    if *offset + sz > in_bytes.len() {
      return Err(CodecError::BinaryInputTooShort)
    }
    let bin = &in_bytes[*offset..(*offset+sz)];
    let py_bytes = PyBytes::new(self.py, bin);

    *offset += sz;
    let remaining = &in_bytes[*offset..in_bytes.len()];
    Ok((py_bytes.into_object(), remaining))
  }
}


pub fn wrap_decode_result(
  py: Python,
  result_pair: Result<(PyObject, &[u8]), CodecError>) -> Result<PyObject, CodecError>
{
  match result_pair {
    Ok((result, tail)) => {
      let py_tail = PyBytes::new(py, tail);
      let elems: Vec<PyObject> = vec![result, py_tail.into_object()];
      let result = PyTuple::new(py, &elems);
      Ok(result.into_object())
    }
    Err(e) => Err(e),
  }
}