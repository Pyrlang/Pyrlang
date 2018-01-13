import gevent

from gevent import monkey
monkey.patch_all()

from Pyrlang import Node, Atom


def main():
    node = Node("py@127.0.0.1", "COOKIE")
    node.start()

    pid = node.register_new_process(None)
    node.send(pid, (Atom('erl@127.0.0.1'), Atom('shell')), Atom('hello'))

    while True:
        # Sleep gives other greenlets time to run
        gevent.sleep(0.1)


if __name__ == "__main__":
    main()
