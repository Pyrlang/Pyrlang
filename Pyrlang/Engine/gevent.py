import gevent

from Pyrlang.Engine.engine import BaseEngine


class GeventEngine(BaseEngine):
    """ Compatibility driver for Gevent.
        Create it before creating Node and pass as a named argument 'engine'
    """

    def __init__(self):
        from gevent import monkey
        monkey.patch_all()

    @staticmethod
    def sleep(seconds: float):
        gevent.sleep(seconds)
