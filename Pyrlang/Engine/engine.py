class BaseEngine:
    def sleep(self, seconds: float):
        raise NotImplementedError()

    def queue_new(self):
        raise NotImplementedError()

    def queue_get(self, q):
        raise NotImplementedError()
