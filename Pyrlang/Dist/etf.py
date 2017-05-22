from __future__ import print_function

from zlib import decompressobj

from Pyrlang import term
from Pyrlang.Dist import util

ETF_VERSION_TAG = 131

TAG_COMPRESSED = 80
TAG_SMALL_INT = 97
TAG_INT = 98
TAG_ATOM_EXT = 100
TAG_PID_EXT = 103
TAG_SMALL_TUPLE_EXT = 104
TAG_LARGE_TUPLE_EXT = 105
TAG_NIL_EXT = 106
TAG_STRING_EXT = 107
TAG_LIST_EXT = 108
TAG_NEW_REF_EXT = 114


class ETFDecodeException(Exception):
    pass


def incomplete_data():
    raise ETFDecodeException("Incomplete data")


def binary_to_term(data: bytes):
    """ Strip 131 header and unpack if the data was compressed
    """
    if data[0] != ETF_VERSION_TAG:
        raise ETFDecodeException("Unsupported external term version")

    # TODO: Compressed tag
    if data[1] == TAG_COMPRESSED:
        do = decompressobj()
        decomp_size = util.u32(data, 2)
        decomp = do.decompress(data[6:]) + do.flush()
        if len(decomp) != decomp_size:
            # Data corruption?
            raise ETFDecodeException("Compressed size mismatch with actual")

        return binary_to_term_2(decomp)

    return binary_to_term_2(data[1:])


def binary_to_term_2(data: bytes):
    """ Proceed decoding after leading tag has been checked and removed
        :param data: Bytes containing encoded term without 131 header
        :return: Tuple (Value, TailBytes)
    """
    tag = data[0]

    if tag == TAG_ATOM_EXT:
        len_data = len(data)
        if len_data < 3:
            return incomplete_data()

        length = util.u16(data, 1) + 3
        if length > len_data:
            return incomplete_data()

        name = data[3:length]
        if name == b'true':
            return True, data[length:]
        if name == b'false':
            return False, data[length:]
        if name == b'undefined':
            return None, data[length:]

        return term.Atom(name.decode('utf8')), data[length:]

    if tag == TAG_NIL_EXT:
        return [], data[1:]

    if tag == TAG_STRING_EXT:
        len_data = len(data)
        if len_data < 3:
            return incomplete_data()

        length = util.u16(data, 1) + 3
        if length < len_data:
            return incomplete_data()

        return data[3:length].decode("utf8"), data[length:]

    if tag == TAG_LIST_EXT:
        length = util.u32(data, 1)
        result = term.List()
        tail = data[5:]
        while length > 0:
            term1, tail = binary_to_term_2(tail)
            result.append(term1)
            length -= 1

        # Read list tail and set it
        list_tail, tail = binary_to_term_2(tail)
        result.set_tail(list_tail)

        return result, tail

    if tag == TAG_SMALL_TUPLE_EXT:
        length = data[1]
        result = []
        tail = data[2:]
        while length > 0:
            term1, tail = binary_to_term_2(tail)
            result.append(term1)
            length -= 1

        return tuple(result), tail

    if tag == TAG_LARGE_TUPLE_EXT:
        length = util.u32(data, 1)
        result = []
        tail = data[5:]
        while length > 0:
            term1, tail = binary_to_term_2(tail)
            result.append(term1)
            length -= 1

        return tuple(result), tail

    if tag == TAG_SMALL_INT:
        return data[1], data[2:]

    if tag == TAG_INT:
        return util.i32(data, 1), data[5:]

    if tag == TAG_PID_EXT:
        node, tail = binary_to_term_2(data[1:])
        id1 = util.u32(tail, 0)
        serial = util.u32(tail, 4)
        creation = tail[8]

        pid = term.Pid(node=node, id=id1, serial=serial, creation=creation)
        return pid, tail[9:]

    if tag == TAG_NEW_REF_EXT:
        term_len = util.u16(data, 1)
        node, tail = binary_to_term_2(data[3:])
        creation = tail[0]
        id_len = 4 * term_len
        id1 = tail[1:id_len + 1]

        ref = term.Reference(node=node, creation=creation, id=id1)
        return ref, tail[id_len + 1:]

    raise ETFDecodeException("Unknown tag %d" % data[0])


__all__ = ['binary_to_term']
