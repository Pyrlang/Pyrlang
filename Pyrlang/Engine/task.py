class Task:
    """ A task adapter which makes it possible to create tasks in Gevent and
        Asyncio. Class which inherits Task must implement ``task_enter``.
    """

    def __init__(self):
        pass

    def task_loop(self) -> bool:
        """ This is called periodically by the Task (async engine adapter).
            Returning True will continue its lifetime.
        """
        return True
