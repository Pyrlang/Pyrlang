use cpython::*;

use super::errors::*;

/// Get dict value with string key, expect it to be string too, or return
/// the default value.
pub fn get_str_opt(py: Python, opts: PyDict,
               optname: &str, default: &str) -> CodecResult<String>
{
  match opts.get_item(py, optname) {
    Some(val) => {
      let py_str: PyString = PyString::extract(py, &val)?;
      Ok(py_str.to_string_lossy(py).into_owned())
    },
    None => {
      Ok(default.to_string())
    },
  }
}