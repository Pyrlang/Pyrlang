//use byte::{BytesExt};
use byteorder::{WriteBytesExt, BigEndian};
use cpython::*;
use std::{i32, u8, u16};
use std::io::{Write};

use super::consts;
use super::errors::*;


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
      "list" => {
        let as_list = PyList::extract(self.py, &term)?;
        self.write_list_no_tail(&as_list);
        self.data.push(consts::TAG_NIL_EXT);
        return Ok(())
      },
      "int" => {
        let val: i64 = FromPyObject::extract(self.py, term)?;
        return self.write_int(val)
      },
      "Atom" => {
        return self.write_atom(&term)
      },
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
      other => {
        println!("Don't know how to encode '{}'", type_name);
        return Err(CodecError::NotImplEncodeForType { t: type_name })
      }
    };
  }


  /// Writes list tag with elements, but no tail element (NIL or other). Ensure
  /// that the calling code is writing either a NIL or a tail term.
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


  fn write_int(&mut self, val: i64) -> CodecResult<()> {
//    let val: i64 = py_i.value();
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


  /// Encode a UTF-8 Atom
  fn write_atom(&mut self, py_atom: &PyObject) -> CodecResult<()> {
    let py_text0 = py_atom.getattr(self.py, "text_")?;
    let py_text: PyString = PyString::extract(self.py, &py_text0)?;
    let text = py_text.to_string(self.py)?;
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