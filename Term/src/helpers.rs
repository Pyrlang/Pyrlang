use cpython::*;

use super::errors::*;

/// Get dict value with string key, expect it to be string too, or return
/// the default value.
pub fn get_str_opt(py: Python, opts: &PyDict,
                   optname: &str, default: &str) -> CodecResult<String>
{
  match opts.get_item(py, optname) {
    Some(val) => {
      let py_str: PyString = PyString::extract(py, &val)?;
      let s = py_str.to_string_lossy(py).into_owned();
      Ok(s)
    },
    None => {
      Ok(default.to_string())
    },
  }
}


/// Given a dict or a possibly None, return dict
pub fn maybe_dict(py: Python, dict_or_none: PyObject) -> PyDict {
  if dict_or_none == py.None() {
    PyDict::new(py)
  } else {
    PyDict::extract(py, &dict_or_none).unwrap()
  }
}
