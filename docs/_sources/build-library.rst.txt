Building the Library
====================

To run pyrlang you need to have term installed. Term is an external package
that pyrlang uses to encode / decode Erlangs `external term fomrat
<http://erlang
.org/doc/apps/erts/erl_ext_dist.html>`__. Install it with:

.. code-block:: console

  pip3 install pyrlang-term

To install Pyrlang you checkout the project and install it:

.. code-block:: console

  git clone git@github.com:Pyrlang/Pyrlang.git
  cd Pyrlang
  pip install .


Install requirements via: :code:`pip3 install -r requirements.txt`, this will
also
pull requirements for Sphinx documentation generator (not needed if you only
use the library).

To build the docs you need to install Sphinx and some other pacakges you can
do that with the requirements file:

.. code-block:: console

  pip install -r requirements.txt

Source for the documentation is in the ``docs-src/`` directory. To generate
HTML, run :code:`make docs` and the ``docs`` directory will be deleted and
recreated.

