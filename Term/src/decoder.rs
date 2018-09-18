use cpython::*;
use byte::BytesExt;
use byte::ctx::Str;
use compress::zlib;
use std::io::{Read, BufReader};
use std::str;

use super::helpers;
use super::helpers::{AtomRepresentation, ByteStringRepresentation};
use super::consts;
use super::errors::*;


#[derive(Copy, Clone, Eq, PartialEq)]
enum Encoding {
  Latin1,
  UTF8
}


pub struct Decoder<'a> {
  py: Python<'a>, // Python instance will live at least as long as Decoder
  atom_representation: AtomRepresentation,
  bytestring_repr: ByteStringRepresentation,

  cached_atom_pyclass: Option<PyObject>,
  cached_pid_pyclass: Option<PyObject>,
  cached_ref_pyclass: Option<PyObject>,
  cached_fun_pyclass: Option<PyObject>,
}


impl <'a> Decoder<'a> {
  /// Create decoder instance. Parse options.
  pub fn new(py: Python, opts: PyObject) -> CodecResult<Decoder> {
    // If opts is None, make it empty Dict, otherwise take it as PyDict
    let opts1 = helpers::maybe_dict(py, opts);

    let aopt = helpers::get_atom_opt(py, &opts1)?;
    let s8opt = helpers::get_byte_str_opt(py, &opts1)?;

    Ok(Decoder {
      py,
      atom_representation: aopt,
      bytestring_repr: s8opt,
      cached_atom_pyclass: None,
      cached_pid_pyclass: None,
      cached_ref_pyclass: None,
      cached_fun_pyclass: None,
    })
  }


  /// Strip 131 byte header and uncompress if the data was compressed.
  /// Return: PyTuple(PyObject, Bytes) or CodecError
  pub fn decode_with_131tag(&mut self, in_bytes: &[u8]) -> CodecResult<PyObject>
  {
    let offset = &mut 0;

    let pre_tag = in_bytes.read_with::<u8>(offset, byte::BE)?;
    if pre_tag != consts::ETF_VERSION_TAG {
      return Err(CodecError::UnsupportedETFVersion)
    } else if in_bytes.is_empty() {
      return Err(CodecError::EmptyInput)
    }

    // Read first byte of term, it might be a compressed term marker
    let tag = in_bytes.read_with::<u8>(offset, byte::BE)?;
    if tag == consts::TAG_COMPRESSED {
      let decomp_size = in_bytes.read_with::<u32>(offset, byte::BE)?;

      let tail1 = &in_bytes[*offset..];
      let mut decompressed = Vec::<u8>::new();
      let mut d = zlib::Decoder::new(BufReader::new(tail1));
      d.read_to_end(&mut decompressed).unwrap();
      if decompressed.len() != decomp_size as usize {
        return Err(CodecError::CompressedSizeMismatch)
      }

      let r1 = self.decode(decompressed.as_ref());
      return wrap_decode_result(self.py, r1)
    }

    // Second byte was not consumed, so restart parsing from the second byte
    let tail2 = &in_bytes[1..];
    let r2 = self.decode(tail2);
    wrap_decode_result(self.py, r2)
  }


  /// Decodes binary External Term Format (ETF) into a Python structure.
  /// Returns: (Decoded object, remaining bytes) or CodecError
  pub fn decode<'inp>(&mut self,
                      in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let tag = in_bytes[0];
    let tail = &in_bytes[1..];
    match tag {
      consts::TAG_ATOM_EXT =>
        self.parse_atom::<u16>(tail, Encoding::Latin1),
      consts::TAG_ATOM_UTF8_EXT =>
        self.parse_atom::<u16>(tail, Encoding::UTF8),
      consts::TAG_SMALL_ATOM_EXT =>
        self.parse_atom::<u8>(tail, Encoding::Latin1),
      consts::TAG_SMALL_ATOM_UTF8_EXT =>
        self.parse_atom::<u8>(tail, Encoding::UTF8),
      consts::TAG_BINARY_EXT => self.parse_binary(tail),
      consts::TAG_NIL_EXT => {
        let empty_list = PyList::new(self.py, empty::slice());
        Ok((empty_list.into_object(), tail))
      },
      consts::TAG_LIST_EXT => self.parse_list(tail),
      consts::TAG_STRING_EXT => self.parse_string(tail), // 16-bit sz bytestr
      consts::TAG_SMALL_UINT => self.parse_number::<u8>(tail),
      consts::TAG_INT => self.parse_number::<i32>(tail),
      consts::TAG_NEW_FLOAT_EXT => self.parse_number::<f64>(tail),
      consts::TAG_MAP_EXT => self.parse_map(tail),
      consts::TAG_SMALL_TUPLE_EXT => {
        let arity = tail[0] as usize;
        self.parse_tuple(&in_bytes[2..], arity)
      },
      consts::TAG_LARGE_TUPLE_EXT => {
        let arity = tail.read_with::<u32>(&mut 0usize, byte::BE)?;
        self.parse_tuple(&in_bytes[5..], arity as usize)
      },
      consts::TAG_PID_EXT => self.parse_pid(tail),
      consts::TAG_NEW_REF_EXT => self.parse_ref(tail),
      consts::TAG_NEW_FUN_EXT => self.parse_fun(tail),
      _ => Err(CodecError::UnknownTermTagByte { b: tag }),
    }
  }


  /// Return cached value of atom class used for decoding. Otherwise if not
  /// found - import and cache it locally.
  fn get_atom_pyclass(&mut self) -> PyObject {
    match &self.cached_atom_pyclass {
      Some(ref a) => a.clone_ref(self.py),
      None => {
        let atom_m = self.py.import("Term.atom").unwrap();
        let atom_cls = atom_m.get(self.py, "Atom").unwrap();
        self.cached_atom_pyclass = Some(atom_cls.clone_ref(self.py));
        atom_cls
      },
    }
  }


  /// Return cached value of Pid class used for decoding. Otherwise if not
  /// found - import and cache it locally.
  fn get_pid_pyclass(&mut self) -> PyObject {
    match &self.cached_pid_pyclass {
      Some(ref a) => a.clone_ref(self.py),
      None => {
        let pid_m = self.py.import("Term.pid").unwrap();
        let pid_cls = pid_m.get(self.py, "Pid").unwrap();
        self.cached_pid_pyclass = Some(pid_cls.clone_ref(self.py));
        pid_cls
      },
    }
  }


  /// Return cached value of Reference class used for decoding. Otherwise if not
  /// found - import and cache it locally.
  fn get_ref_pyclass(&mut self) -> PyObject {
    match &self.cached_ref_pyclass {
      Some(ref a) => a.clone_ref(self.py),
      None => {
        let ref_m = self.py.import("Term.reference").unwrap();
        let ref_cls = ref_m.get(self.py, "Reference").unwrap();
        self.cached_ref_pyclass = Some(ref_cls.clone_ref(self.py));
        ref_cls
      },
    }
  }


  /// Return cached value of Fun class used for decoding. Otherwise if not
  /// found - import and cache it locally.
  fn get_fun_pyclass(&mut self) -> PyObject {
    match &self.cached_fun_pyclass {
      Some(ref a) => a.clone_ref(self.py),
      None => {
        let fun_m = self.py.import("Term.fun").unwrap();
        let fun_cls = fun_m.get(self.py, "Fun").unwrap();
        self.cached_fun_pyclass = Some(fun_cls.clone_ref(self.py));
        fun_cls
      },
    }
  }


  #[inline]
  fn parse_number<'inp, T>(&self, in_bytes: &'inp [u8])
    -> CodecResult<(PyObject, &'inp [u8])>
    where T: byte::TryRead<'inp, byte::ctx::Endian> + ToPyObject
  {
    let offset = &mut 0usize;
    let val = in_bytes.read_with::<T>(offset, byte::BE)?;
    let py_val = val.to_py_object(self.py);
    Ok((py_val.into_object(), &in_bytes[*offset..]))
  }


//  #[inline]
//  fn parse_bin_float<'inp>(&self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
//  {
//    let val = in_bytes[1];
//    let py_val = val.to_py_object(self.py);
//    Ok((py_val.into_object(), &in_bytes[2..]))
//  }


  /// Parses bytes after Atom tag (100) or Atom Utf8 (118)
  /// Returns: Tuple (string | bytes | Atom object, remaining bytes)
  #[inline]
  fn parse_atom<'inp, T>(&mut self, in_bytes: &'inp [u8],
                         _coding: Encoding) -> CodecResult<(PyObject, &'inp [u8])>
    where usize: std::convert::From<T>,
          T: byte::TryRead<'inp, byte::ctx::Endian>
  {
    let offset = &mut 0usize;
    let sz = in_bytes.read_with::<T>(offset, byte::BE)?;
    let txt = in_bytes.read_with::<&str>(offset,
                                         Str::Len(usize::from(sz)))?;

    let result = self.create_atom(txt)?.into_object();
    let remaining = &in_bytes[*offset..];
    Ok((result, remaining))
  }


  // TODO: Make 3 functions and store fun pointer
  #[inline]
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
        let atom_obj = self.get_atom_pyclass();
        Ok(atom_obj.call(self.py, (txt,), None)?)
      },
    } // match
  }


  /// Given input _after_ binary tag, parse remaining bytes
  #[inline]
  fn parse_binary<'inp>(&self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let offset = &mut 0usize;
    let sz = in_bytes.read_with::<u32>(offset, byte::BE)? as usize;
    if *offset + sz > in_bytes.len() {
      return Err(CodecError::BinaryInputTooShort)
    }
    let bin = &in_bytes[*offset..(*offset+sz)];
    let py_bytes = PyBytes::new(self.py, bin);

    *offset += sz;
    let remaining = &in_bytes[*offset..];
    Ok((py_bytes.into_object(), remaining))
  }


  /// Given input _after_ string tag, parse remaining bytes as an ASCII string
  #[inline]
  fn parse_string<'inp>(&self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let offset = &mut 0usize;
    let sz = in_bytes.read_with::<u16>(offset, byte::BE)? as usize;
    if *offset + sz > in_bytes.len() {
      return Err(CodecError::StrInputTooShort)
    }

    let result = match self.bytestring_repr {
      ByteStringRepresentation::Str => {
        let rust_str = in_bytes.read_with::<&str>(
          offset, Str::Len(sz as usize)
        )?;
        PyString::new(self.py, rust_str).into_object()
      },
      ByteStringRepresentation::Bytes => {
        let offset1 = *offset;
        *offset += sz;
        PyBytes::new(self.py, &in_bytes[offset1..(offset1 + sz)]).into_object()
      },
    };

    let remaining = &in_bytes[*offset..];
    Ok((result, remaining))
  }


  /// Given input _after_ the list tag, parse the list elements and tail
  #[inline]
  fn parse_list<'inp>(&mut self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])> {
    let offset = &mut 0usize;
    let sz = in_bytes.read_with::<u32>(offset, byte::BE)? as usize;

    let mut lst = Vec::<PyObject>::with_capacity(sz);

    // Read list elements, one by one
    let mut tail = &in_bytes[*offset..];
    for _i in 0..sz {
      let (val, new_tail) = self.decode(tail)?;
      tail = new_tail;
      lst.push(val);
    }

    let py_lst = PyList::new(self.py, lst.as_ref());

    // Check whether last element is a NIL, or something else
    if tail[0] == consts::TAG_NIL_EXT {
      // We are looking at a proper list, so just return the result
      Ok((py_lst.into_object(), &tail[1..]))
    } else {
      // We are looking at an improper list
      let (tail_val, tail_bytes) = self.decode(tail)?;
      let py_pair = PyTuple::new(self.py, &[py_lst.into_object(), tail_val]);
      Ok((py_pair.into_object(), tail_bytes))
    }
  }


  /// Given input _after_ the TAG_MAP_EXT byte, parse map key/value pairs.
  #[inline]
  fn parse_map<'inp>(&mut self,
                     in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let offset = &mut 0usize;
    let arity = in_bytes.read_with::<u32>(offset, byte::BE)? as usize;

    let mut result = PyDict::new(self.py);

    // Read key/value pairs two at a time
    let mut tail = &in_bytes[*offset..];
    for _i in 0..arity {
      let (py_key, tail1) = self.decode(tail)?;
      let (py_val, tail2) = self.decode(tail1)?;
      tail = tail2;
      result.set_item(self.py, py_key, py_val);
    }

    Ok((result.into_object(), tail))
  }


  /// Given input _after_ the TAG_SMALL_TUPLE_EXT or the TAG_TUPLE_EXT byte,
  /// tuple elements into a vector and create Python tuple.
  #[inline]
  fn parse_tuple<'inp>(&mut self,
                       in_bytes: &'inp [u8],
                       arity: usize) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let mut result = Vec::<PyObject>::with_capacity(arity);

    // Read values one by one
    let mut tail = in_bytes;
    for _i in 0..arity {
      let (py_val, tail1) = self.decode(tail)?;
      tail = tail1;
      result.push(py_val);
    }

    let py_result = PyTuple::new(self.py, result.as_ref());
    Ok((py_result.into_object(), tail))
  }


  /// Given input _after_ the PID tag byte, parse an external pid
  #[inline]
  fn parse_pid<'inp>(&mut self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let (node, tail1) = self.decode(in_bytes)?;
    let offset = &mut 0usize;
    let id: u32 = tail1.read_with::<u32>(offset, byte::BE)?;
    let serial: u32 = tail1.read_with::<u32>(offset, byte::BE)?;
    let creation: u8 = tail1.read_with::<u8>(offset, byte::BE)?;

    let remaining = &tail1[*offset..];
    let pid_obj = self.get_pid_pyclass();
    let py_pid = pid_obj.call(self.py, (node, id, serial, creation), None)?;
    Ok((py_pid.into_object(), remaining))
  }


  /// Given input _after_ the Reference tag byte, parse an external reference
  #[inline]
  fn parse_ref<'inp>(&mut self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let offset = &mut 0usize;
    let term_len: u16 = in_bytes.read_with::<u16>(offset, byte::BE)?;

    let (node, tail1) = self.decode(&in_bytes[*offset..])?;

    let creation: u8 = tail1[0];
    let last_index = 1 + (term_len as usize) * 4;
    let id: &[u8] = &tail1[1..last_index];

    let remaining = &tail1[last_index..];
    let ref_obj = self.get_ref_pyclass();
    let py_ref = ref_obj.call(self.py, (node, creation, id), None)?;
    Ok((py_ref.into_object(), remaining))
  }


  /// Given input _after_ the Fun tag byte, parse a fun (not useful in Python
  /// but we store all parts of it and can reconstruct it, if it will be sent out)
  #[inline]
  fn parse_fun<'inp>(&mut self, in_bytes: &'inp [u8]) -> CodecResult<(PyObject, &'inp [u8])>
  {
    let offset = &mut 0usize;
    let size = in_bytes.read_with::<u32>(offset, byte::BE)?;
    let arity = in_bytes.read_with::<u8>(offset, byte::BE)? as usize;

    let uniq_md5 = &in_bytes[*offset..*offset+16];
    *offset += 16;

    let index = in_bytes.read_with::<u32>(offset, byte::BE)?;
    let num_free = in_bytes.read_with::<u32>(offset, byte::BE)?;

    let tail0 = &in_bytes[*offset..];
    let (module, tail1) = self.decode(tail0)?;
    let (old_index, tail2) = self.decode(tail1)?;
    let (old_uniq, tail3) = self.decode(tail2)?;
    let (pid, tail4) = self.decode(tail3)?;

    // Decode num_free free variables following after pid
    let mut frozen_vars = Vec::<PyObject>::with_capacity(arity);
    let mut tail = tail4;
    for _i in 0..num_free {
      let (py_val, tail_new) = self.decode(tail)?;
      tail = tail_new;
      frozen_vars.push(py_val);
    }
    let py_frozen_vars = PyTuple::new(self.py, frozen_vars.as_ref());

    let fun_obj = self.get_fun_pyclass();
    let py_fun = fun_obj.call(self.py,
                              (module, arity, pid,
                               index, uniq_md5,
                               old_index, old_uniq,
                               py_frozen_vars.into_object()),
                              None)?;
    Ok((py_fun.into_object(), tail))
  }

}
// end impl


pub fn wrap_decode_result(
  py: Python,
  result_pair: Result<(PyObject, &[u8]), CodecError>) -> Result<PyObject, CodecError>
{
  match result_pair {
    Ok((result, tail)) => {
      let py_tail = PyBytes::new(py, tail);
      let result = PyTuple::new(py, &[result, py_tail.into_object()]);
      Ok(result.into_object())
    }
    Err(e) => Err(e),
  }
}