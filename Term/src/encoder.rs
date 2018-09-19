//use byte::{BytesExt};
use byteorder::{WriteBytesExt, BigEndian};
use cpython::*;
use std::{i32, u8, u16};
use std::io::{Write};

use super::consts;
use super::errors::*;
use std::borrow::Cow;


pub struct Encoder<'a> {
  pub py: Python<'a>, // Python instance will live at least as long as Encoder
  pub data: Vec<u8>,
}


impl<'a> Encoder<'a> {
  pub fn new(py: Python) -> Encoder {
    Encoder {
      py,
      data: Vec::with_capacity(32),
    }
  }


  pub fn encode(&mut self, term: &PyObject) -> CodecResult<()> {
    let type_name = term.get_type(self.py).name(self.py).into_owned();
    let type_name_ref: &str = type_name.as_ref();
    match type_name_ref {
      "int" => {
        let val: i64 = FromPyObject::extract(self.py, term)?;
        return self.write_int(val)
      },
      "float" => {
        let val: f64 = FromPyObject::extract(self.py, term)?;
        return self.write_float(val)
      },
      "list" => {
        let as_list = PyList::extract(self.py, &term)?;
        self.write_list_no_tail(&as_list);
        self.data.push(consts::TAG_NIL_EXT);
        return Ok(())
      },
      "dict" => {
        let as_dict = PyDict::extract(self.py, &term)?;
        self.write_dict(&as_dict);
        return Ok(())
      },
      "Atom" => return self.write_atom(&term),
      "str" => {
        let as_str = PyString::extract(self.py, &term)?;
        return self.write_str(&as_str)
      },
      "ImproperList" => {
        let elements0 = term.getattr(self.py, "elements_")?;
        let elements = PyList::extract(self.py, &elements0)?;
        let tail = term.getattr(self.py, "tail_")?;
        self.write_list_no_tail(&elements);
        return self.encode(&tail)
      },
      "Pid" => return self.write_pid(&term),
      "Reference" => return self.write_ref(&term),
      "bytes" => {
        let py_bytes = PyBytes::extract(self.py, &term)?;
        return self.write_binary(&py_bytes)
      },
      "BitString" => return self.write_bitstring(&term),
      //"Fun" => return self.write_fun(&term),
      other => {
        println!("Don't know how to encode '{}'", type_name);
        return Err(CodecError::NotImplEncodeForType { t: type_name })
      }
    };
  }


  /// Writes list tag with elements, but no tail element (NIL or other). Ensure
  /// that the calling code is writing either a NIL or a tail term.
  #[inline]
  fn write_list_no_tail(&mut self, list: &PyList) -> CodecResult<()> {
    let size = list.len(self.py);
    self.data.push(consts::TAG_LIST_EXT);
    self.data.write_u32::<BigEndian>(size as u32);

    for i in 0..size {
      let item = list.get_item(self.py, i);
      self.encode(&item);
    }
    Ok(())
  }


  /// Writes Erlang map from Python dict.
  #[inline]
  fn write_dict(&mut self, py_dict: &PyDict) -> CodecResult<()> {
    let size = py_dict.len(self.py);
    self.data.push(consts::TAG_MAP_EXT);
    self.data.write_u32::<BigEndian>(size as u32);

    for (py_key, py_value) in py_dict.items(self.py) {
      self.encode(&py_key);
      self.encode(&py_value);
    }
    Ok(())
  }


  #[inline]
  fn write_int(&mut self, val: i64) -> CodecResult<()> {
    if val >= 0 && val <= u8::MAX as i64 {
      self.data.push(consts::TAG_SMALL_UINT);
      self.data.push(val as u8);
    } else if val >= i32::MIN as i64
        && val <= i32::MAX as i64 {
      self.data.push(consts::TAG_INT);
      self.data.write_i32::<BigEndian>(val as i32);
    } else {
      return Err(CodecError::IntegerEncodingRange {i: val})
    }

    Ok(())
  }


  #[inline]
  fn write_float(&mut self, val: f64) -> CodecResult<()> {
    self.data.push(consts::TAG_NEW_FLOAT_EXT);
    self.data.write_f64::<BigEndian>(val);
    Ok(())
  }


  /// Encode a UTF-8 Atom
  #[inline]
  fn write_atom(&mut self, py_atom: &PyObject) -> CodecResult<()> {
    let py_text0 = py_atom.getattr(self.py, "text_")?;
    let py_text: PyString = PyString::extract(self.py, &py_text0)?;
    let text = py_text.to_string(self.py)?;
    self.write_atom_from_string(text)
  }


  /// Helper which writes an atom from a PyString's Copy-on-write string
  fn write_atom_from_string(&mut self, text: Cow<str>) -> CodecResult<()> {
    let byte_array: &[u8] = text.as_ref().as_ref();
    let str_byte_length: usize = byte_array.len();

    if str_byte_length <= u8::MAX as usize {
      self.data.push(consts::TAG_SMALL_ATOM_UTF8_EXT);
      self.data.push(str_byte_length as u8); // 8bit length
      self.data.write(byte_array); // write &[u8] string content
    } else if str_byte_length <= u16::MAX as usize {
      self.data.push(consts::TAG_ATOM_UTF8_EXT);
      self.data.write_u16::<BigEndian>(str_byte_length as u16); // 16bit length
      self.data.write(byte_array); // write &[u8] string content
    } else {
      return Err(CodecError::AtomTooLong)
    }

    Ok(())
  }


  /// Encode a UTF-8 string
  #[inline]
  fn write_str(&mut self, py_str: &PyString) -> CodecResult<()> {
    let text = py_str.to_string(self.py)?;
    let byte_array: &[u8] = text.as_ref().as_ref();
    let str_byte_length: usize = byte_array.len();
    let can_be_encoded_as_bytes = can_be_encoded_as_byte_string(&text);

    if str_byte_length <= u8::MAX as usize && can_be_encoded_as_bytes {
      // Create an optimised byte-array structure and push bytes
      self.data.push(consts::TAG_STRING_EXT);
      self.data.write_u16::<BigEndian>(str_byte_length as u16); // 16bit length
      self.data.write(byte_array); // write &[u8] string content
    } else {
      // Create a list structure and push each codepoint as an integer
      self.data.push(consts::TAG_LIST_EXT);
      let chars_count = text.chars().count();
      self.data.write_u32::<BigEndian>(chars_count as u32); // chars, not bytes!
      for (_i, ch) in text.char_indices() {
        self.write_int(ch as i64)?
      }
      self.data.push(consts::TAG_NIL_EXT) // list terminator
    }

    Ok(())
  }


  /// Encode a Pid
  #[inline]
  fn write_pid(&mut self, py_pid: &PyObject) -> CodecResult<()> {
    let node_name = PyString::extract(
      self.py, &py_pid.getattr(self.py, "node_name_")?
    )?;

    let py_id = py_pid.getattr(self.py, "id_")?;
    let id: u32 = FromPyObject::extract(self.py, &py_id)?;

    let py_serial = py_pid.getattr(self.py, "serial_")?;
    let serial: u32 = FromPyObject::extract(self.py, &py_serial)?;

    let py_creation = py_pid.getattr(self.py, "creation_")?;
    let creation: u8 = FromPyObject::extract(self.py, &py_creation)?;

    self.data.push(consts::TAG_PID_EXT);
    self.write_atom_from_string(node_name.to_string(self.py)?);
    self.data.write_u32::<BigEndian>(id);
    self.data.write_u32::<BigEndian>(serial);
    self.data.push(creation);

    Ok(())
  }


  /// Encode a Reference
  #[inline]
  fn write_ref(&mut self, py_ref: &PyObject) -> CodecResult<()> {
    let node_name = PyString::extract(
      self.py, &py_ref.getattr(self.py, "node_name_")?
    )?;

    let py_id: PyBytes = PyBytes::extract(
      self.py, &py_ref.getattr(self.py, "id_")?
    )?;
    let id = py_id.data(self.py);

    let py_creation = py_ref.getattr(self.py, "creation_")?;
    let creation: u8 = FromPyObject::extract(self.py, &py_creation)?;

    self.data.push(consts::TAG_NEW_REF_EXT);
    self.data.write_u16::<BigEndian>((id.len() / 4) as u16);
    self.write_atom_from_string(node_name.to_string(self.py)?);
    self.data.push(creation);
    self.data.write(id);

    Ok(())
  }


  /// Encode a binary (byte-string)
  #[inline]
  fn write_binary(&mut self, py_bytes: &PyBytes) -> CodecResult<()> {
    let data: &[u8] = py_bytes.data(self.py);
    self.data.push(consts::TAG_BINARY_EXT);
    self.data.write_u32::<BigEndian>(data.len() as u32);
    self.data.write(data);
    Ok(())
  }


  /// Encode a Binary bit-string (last byte has less than 8 bits)
  #[inline]
  fn write_bitstring(&mut self, py_bits: &PyObject) -> CodecResult<()> {
    let py_bytes = PyBytes::extract(
      self.py, &py_bits.getattr(self.py, "value_")?
    )?;
    let data: &[u8] = py_bytes.data(self.py);

    let py_lbb = py_bits.getattr(self.py, "last_byte_bits_")?;
    let last_byte_bits: u8 = FromPyObject::extract(self.py, &py_lbb)?;

    self.data.push(consts::TAG_BIT_BINARY_EXT);
    self.data.write_u32::<BigEndian>(data.len() as u32);
    self.data.push(last_byte_bits);
    self.data.write(data);

    Ok(())
  }

} // end impl


/// Checks first 65535 characters whether they are single-byte and are not
/// extended code points
fn can_be_encoded_as_byte_string(s: &str) -> bool {
  for (i, ch) in s.char_indices() {
    if i > u16::MAX as usize {
      return false // too long, so result is false
    }
    if ch as u32 > u8::MAX as u32 {
      return false // is a unicode codepoint with value larger than 255
    }
  }
  return true // will fit in a 255-byte latin-1 string
}
