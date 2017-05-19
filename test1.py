from gevent import monkey
monkey.patch_all()

import Pyrlang as pyr


def main():
    pyr.init()
    node = pyr.ErlNode("py@127.0.0.1", "COOKIE")
    # Block until the node has finished running
    node.run()


if __name__ == "__main__":
    main()
