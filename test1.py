import gevent
from gevent import monkey
monkey.patch_all()

import Pyrlang as pyr


def main():
    pyr.init()
    node = pyr.ErlNode("py@127.0.0.1", "COOKIE")
    node.start()

    while True:
        # Sleep gives other greenlets time to run
        gevent.sleep(0.1)


if __name__ == "__main__":
    main()
