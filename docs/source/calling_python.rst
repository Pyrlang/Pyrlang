Calling Python Remotely
=======================

There is the regular ordinary way to call remote functions from Erlang, using
``rpc:call`` which is handled by :py:class:`~Pyrlang.rex.Rex` process in Pyrlang
or by ``rex`` (named process) in Erlang.

These calls return the result immediately and do not allow multiple
calls without transferring unnecessary amounts of data forth and back.

Pyrlang implements notebook-style calls in :py:mod:`~Pyrlang.Notebook.notebook`
where results of your calls from Erlang to Python are stored on Python side
until they are needed. You can substitute stored values into following calls.

Enter the ``py.erl`` helper module, which you can drop into your Erlang project
and use as a library.

.. code-block:: shell

    # Your Unix shell: Start Erlang node with name and cookie
    $ erl -name erl@127.0.0.1 -setcookie COOKIE

.. code-block:: erlang

    %% Create a remote notebook object (context) on Python side
    Ctx  = py:new_context('py@127.0.0.1'),

    %% Import datetime and call datetime.now(), kwargs are empty by default.
    %% Note that binaries, ASCII strings and atoms all work.
    %% First element in the list is module name to be imported
    DT1  = py:call(Ctx, [<<"datetime">>, "datetime", now], []),
    timer:sleep(2000),

    %% Import datetime and call datetime.now() again but now with kwargs #{}
    DT2  = py:call(Ctx, [datetime, datetime, <<"now">>], [], #{}),

    %% Subtract two datetimes
    Diff = py:call(Ctx, [DT2, '__sub__'], [DT1], #{}),

    %% Call Diff.total_seconds() and retrieve the value without storing it
    %% in remote history.
    Result1 = py:call(Ctx, [Diff, <<"total_seconds">>], [], #{}, #{immediate => true}),

    %% Or retrieve the diff and store it remotely then retrieve
    Result2Ref = py:call(Ctx, [Diff, <<"total_seconds">>], []),
    Result2 = py:retrieve(Ctx, Result2Ref),

    %% Done with the remote context. Remote notebook object will be dropped.
    py:destroy(Ctx).


API Quick Description
---------------------

Erlang module ``py`` contains the following functions which might be useful:

.. code-block:: erlang

    py:new_context(Node).
    py:new_context(Node, Options).

This will perform a remote call to your Python node, and create
:py:class:`~Pyrlang.Notebook.notebook.Notebook` object with default history
limit of 50 values. ``Options`` map can contain key ``history`` with an integer
value if you want to override this default.

.. code-block:: erlang

    py:call(Context, Path, Args).
    py:call(Context, Path, Args, KwArgs).
    py:call(Context, Path, Args, KwArgs, Options).

Performs a remote call to the ``Notebook`` on Python side, which resolves
whether first element of a ``Path`` is a value from previous calculation or
a module name, and then find the function by following remaining items in
the ``Path``.

``Options`` is a dict which can contain keys:

*   ``timeout``: ``int`` (default 5000)
*   ``immediate``: ``bool`` (default false) - setting this to ``true`` will not
    update the remote history and instead will return you the actual value.

On exception you receive Erlang exception with tuple
``{'ExceptionClassName', #{args, traceback}}``.

.. code-block:: erlang

    py:destroy(Context).

Ends life of the remote ``Notebook``.

.. code-block:: erlang

    py:get_type(ValueReference).

For remote value reference its type is known on Erlang side.
Retrieve this type name as a string.