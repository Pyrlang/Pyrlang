def nothing(*args, **kwargs):
    pass


def tty(*args, **kwargs):
    """ Prints args to terminal (console) """
    print(*args, **kwargs)


__all__ = ['tty', 'nothing']
