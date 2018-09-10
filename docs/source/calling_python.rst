Remote Calling Python from Erlang
=================================

There is the regular ordinary way to call remote functions from Erlang, using
``rpc:call`` which is handled by :py:class:`~Pyrlang.rex.Rex` process in Pyrlang
or by ``rex`` (named process) in Erlang.

These calls return the result immediately and do not allow multiple
calls without transferring unnecessary amounts of data forth and back.


Notebook-style Calls
--------------------

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
`````````````````````

Erlang module ``py`` contains the following functions which might be useful:

.. code-block:: erlang

    Context = py:new_context(Node).
    Context = py:new_context(Node, Options).

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

.. note::
    Default ``immediate=False`` flag here differs from default
    ``immediate=True`` for batched calls (scripts).

.. code-block:: erlang

    py:destroy(Context).

Ends life of the remote ``Notebook``.

.. code-block:: erlang

    py:get_type(ValueReference).

For remote value reference its type is known on Erlang side.
Retrieve this type name as a string.


Batching Remote Calls
---------------------

Another extension to Notebook-style calls is **batches**, supported by the same
``py`` module on Erlang side and by the same
:py:class:`~Pyrlang.Notebook.notebook.Notebook` class on Python side.

A batch is a sequence of calls, similar to notebook-style calls, where result
of a previous call can be connected to input of any following call. A batch
is prepared on Erlang side and then can be executed on any or multiple
Python nodes.


.. code-block:: shell

    # Your Unix shell: Start Erlang node with name and cookie
    $ erl -name erl@127.0.0.1 -setcookie COOKIE

.. code-block:: erlang

    %% Create an empty batch and begin adding calls to it
    S0 = py:batch_new(),
    {S1, R1} = py:batch_call(S0, [<<"datetime">>, "datetime", now], []),
    {S2, R2} = py:batch_call(S1, [datetime, datetime, <<"now">>], [], #{}),
    %% Subtract two datetimes
    {S3, Diff} = py:batch_call(S2, [DT2, '__sub__'], [DT1], #{}),

    %% Call Diff.total_seconds() and retrieve the value without storing it
    %% in remote history.
    {S4, _R4} = py:batch_call(S3, [Diff, <<"total_seconds">>]),

    %% Create a remote notebook object (context) on Python side
    Ctx  = py:new_context('py@127.0.0.1'),

    %% will retrieve because immediate=true by default
    Result2 = py:batch_run(Ctx, S4),

    %% Done with the remote context. Remote notebook object will be dropped.
    py:destroy(Ctx).


API Quick Description
`````````````````````

.. code-block:: erlang

    Batch = py:batch_new().

Will create an empty batch with no calls in it.

.. code-block:: erlang

    py:batch_call(Batch, Path, Args) -> {Batch1, ResultRef}.
    py:batch_call(Batch, Path, Args, KwArgs) -> {Batch1, ResultRef}.

Will append another call to the sequence in ``Batch``. Returns a pair of
updated batch and id for referring to its result.

.. code-block:: erlang

    Context = py:new_context(Node).
    Context = py:new_context(Node, Options).

This is same as in notebook-style single remote calls above. We need to create
a context to spawn remote process which will do the job and store the call
result history.

.. code-block:: erlang

    py:batch_run(Context, Batch, Options).

Performs remote execution of call sequence on a given node. You can perform
calls to multiple contexts at different nodes with the same ``Batch``.

``Options`` is a dict which can contain keys:

*   ``timeout``: ``int`` (default 5000)
*   ``immediate``: ``bool`` (default true) - setting this to ``false`` will
    instead update the remote history and return you a value reference. Default
    setting returns you the actual value.

.. note::
    Default ``immediate=True`` flag here differs from default
    ``immediate=False`` for single calls.
