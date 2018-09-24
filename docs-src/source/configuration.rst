Configuring Pyrlang in Runtime
==============================

Set OS env variable ``PYRLANG_ENABLE_LOG_FORMAT`` to one of
``["1", "yes", "true", "on"]``
to enable Pyrlang custom debug logging format (looks like this:
``2018-09-06 10:03:57,019 [Pyrlang] asyncio_engine:120: Listening on ('', 0) (33953)``
) i.e. date, time, logger name, file, line and the message.

Set OS env variable ``PYRLANG_LOG_LEVEL`` to one of
``['CRITICAL', 'ERROR', 'WARNING', 'INFO', 'DEBUG', 'NOTSET']``
to override the default log level of ``INFO``.

For all purposes the code which checks these settings is located in
:py:func:`~pyrlang.util.start_pyrlang`.
