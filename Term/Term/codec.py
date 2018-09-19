""" Adapter module which attempts to import native (Rust) codec implementation
    and then if import fails, uses Python codec implementation which is slower
    but always works.
"""
import logging

LOG = logging.getLogger("Term")


try:
    import native_codec_impl as co_impl

    def decorate(x):
        """ For Native extension i've no idea how to make default arg value """
        def wrapper(val, opt=None):
            x(val, opt)
        return wrapper

except ImportError:
    LOG.warning("Native library import failed, falling back to slower Python impl")
    import Term.py_codec_impl as co_impl

    def decorate(x):
        return x

binary_to_term = decorate(co_impl.binary_to_term)
binary_to_term_2 = decorate(co_impl.binary_to_term_2)

term_to_binary = decorate(co_impl.term_to_binary)
term_to_binary_2 = decorate(co_impl.term_to_binary_2)

PyCodecError = co_impl.PyCodecError

__all__ = ['term_to_binary', 'term_to_binary_2',
           'binary_to_term', 'binary_to_term_2',
           'PyCodecError']
