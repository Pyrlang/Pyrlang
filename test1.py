from gevent import monkey
from Pyrlang import ErlNode


def main():
    monkey.patch_all()

    node = ErlNode("py", "COOKIE")
    # Block until the node has finished running
    node.join()
