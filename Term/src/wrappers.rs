//use cpython::{PyResult};
//
//py_class!(pub class Atom |py| {
//  data text_: String;
//  def __new__(_cls, text: String) -> PyResult<Atom> {
//      Atom::create_instance(py, text)
//  }
//});