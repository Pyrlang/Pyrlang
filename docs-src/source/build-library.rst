Building the Library
====================

If you just want to get going just install pyralng using :code:`pip` the dependancy
:code:`pyrlang-term` will be installed automatically

.. code-block:: console

  pip3 install pyrlang


To install pyrlang from source you need to install term manually. Term is an external package
that pyrlang uses to encode / decode Erlangs
`external term fomrat <http://erlang .org/doc/apps/erts/erl_ext_dist.html>`__. Install it with:

.. code-block:: console

  pip3 install pyrlang-term

To install Pyrlang you checkout the project and install it:

.. code-block:: console

  git clone git@github.com:Pyrlang/Pyrlang.git
  cd Pyrlang
  pip install .



To build the docs you need to install Sphinx and some other pacakges you can
do that with the requirements file:

.. code-block:: console

  pip install -r requirements.txt

Source for the documentation is in the ``docs-src/`` directory. To generate
HTML, run :code:`make docs` and the ``docs`` directory will be deleted and
recreated.

