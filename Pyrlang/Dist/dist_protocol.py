""" Implement shared pieces of Erlang node negotiation and distribution
    protocol
"""

DIST_VSN = 5
DIST_VSN_PAIR = (DIST_VSN, DIST_VSN)
" Supported distribution protocol version (MAX,MIN). "


def dist_version_check(max_min: tuple) -> bool:
    """ Check pair of versions against version which is supported by us

        :type max_min: tuple(int, int)
        :param max_min: (Max, Min) version pair for peer-supported dist version
    """
    return max_min[0] >= DIST_VSN >= max_min[1]

# __all__ = ['DIST_VSN', 'DIST_VSN_PAIR']
