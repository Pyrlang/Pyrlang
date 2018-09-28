Building the Library
====================

Install requirements via: `pip3 install -r requirements.txt`, this will also
pull requirements for Sphinx documentation generator (not needed if you only
use the library).

To operate Pyrlang requires its own Term library (located at
``https://github.com/Pyrlang/Term`` and a couple other libraries:
``gevent`` and ``greenlet`` (dependency of ``gevent``) OR ``asyncio``.

Source for the documentation is in the ``docs-src/`` directory. It uses Sphinx
documentation generator. To generate HTML: run ``make docs`` and the ``docs``
directory will be deleted and recreated..
