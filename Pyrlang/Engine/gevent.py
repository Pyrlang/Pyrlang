import gevent

from Pyrlang.Engine.engine import BaseEngine
from Pyrlang.Engine.task import Task


def task_loop_helper(t: Task):
    while t.task_loop():
        gevent.sleep(0.0)


class GeventEngine(BaseEngine):
    """ Compatibility driver for Gevent.
        Create it before creating Node and pass as argument 'engine' like so:

        e = GeventEngine()
        node = Node(name="py@127.0.0.1", cookie="COOKIE", engine=e)
    """

    def __init__(self):
        from gevent import monkey
        monkey.patch_all()

        # self.loop_ = gevent.get_hub()

    def sleep(self, seconds: float):
        gevent.sleep(seconds)

    @staticmethod
    def start_task(t: Task):
        """ Start task loop helper which will call periodically t.task_loop and
            sleep for 0
        """
        gevent.spawn(lambda: task_loop_helper(t))
