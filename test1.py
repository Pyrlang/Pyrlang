import gevent
from gevent import monkey
monkey.patch_all()

import Pyrlang


def main():
    Pyrlang.init()
    node = Pyrlang.Node("py@127.0.0.1", "COOKIE")
    node.start()

    while True:
        # Sleep gives other greenlets time to run
        gevent.sleep(0.0)


if __name__ == "__main__":
    main()
