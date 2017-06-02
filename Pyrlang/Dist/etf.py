# Copyright 2017, Erlang Solutions Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

""" Module implements encoder and decoder from ETF (Erlang External Term Format)
    used by the network distribution layer.

    Encoding terms takes optional 'options' argument. Default is ``None`` but
    it can be a dictionary with the following optional keys:

    *   "binaries_as_bytes", default False. Ignores bit tail of bit strings and
        returns all Erlang binaries as Python bytes.
    *   "atoms_as_strings", default False. Always converts atoms to Python
        strings. This is potentially faster than using the Atom wrapper class.
"""
from __future__ import print_function

import struct

from zlib import decompressobj

from Pyrlang import term
from Pyrlang.Dist import util

ETF_VERSION_TAG = 131

TAG_NEW_FLOAT_EXT = 70
TAG_BIT_BINARY_EXT = 77
TAG_COMPRESSED = 80
TAG_SMALL_INT = 97
TAG_INT = 98
TAG_FLOAT_EXT = 99
TAG_ATOM_EXT = 100
TAG_PID_EXT = 103
TAG_SMALL_TUPLE_EXT = 104
TAG_LARGE_TUPLE_EXT = 105
TAG_NIL_EXT = 106
TAG_STRING_EXT = 107
TAG_LIST_EXT = 108
TAG_BINARY_EXT = 109
TAG_NEW_FUN_EXT = 112
TAG_NEW_REF_EXT = 114
TAG_SMALL_ATOM_EXT = 115
TAG_MAP_EXT = 116
TAG_ATOM_UTF8_EXT = 118
TAG_SMALL_ATOM_UTF8_EXT = 119


class ETFDecodeException(Exception):
    pass


class ETFEncodeException(Exception):
    pass


def incomplete_data(where=""):
    """ This is called from many places to report incomplete data while
        decoding.
    """
    if where:
        raise ETFDecodeException("Incomplete data")
    else:
        raise ETFDecodeException("Incomplete data at " + where)


def binary_to_term(data: bytes, options: dict = {}):
    """ Strip 131 header and unpack if the data was compressed.

        :param data: The incoming encoded data with the 131 byte
        :param options: See description on top of the module
        :raises ETFDecodeException: when the tag is not 131, when compressed
            data is incomplete or corrupted
    """
    if data[0] != ETF_VERSION_TAG:
        raise ETFDecodeException("Unsupported external term version")

    if data[1] == TAG_COMPRESSED:
        do = decompressobj()
        decomp_size = util.u32(data, 2)
        decomp = do.decompress(data[6:]) + do.flush()
        if len(decomp) != decomp_size:
            # Data corruption?
            raise ETFDecodeException("Compressed size mismatch with actual")

        return binary_to_term_2(decomp, options)

    return binary_to_term_2(data[1:], options)


def _bytes_to_atom(name: bytes, encoding: str, options: dict):
    """ Recognize familiar atom values. """
    if name == b'true':
        return True
    if name == b'false':
        return False
    if name == b'undefined':
        return None

    if options.get("atoms_as_strings", False):
        return name.decode(encoding)

    return term.Atom(text=name.decode(encoding),
                     encoding=encoding)


def binary_to_term_2(data: bytes, options: dict = {}):
    """ Proceed decoding after leading tag has been checked and removed.

        Erlang lists are decoded into ``term.List`` object, whose ``elements_``
        field contains the data, ``tail_`` field has the optional tail and a
        helper function exists to assist with extracting an unicode string.

        Atoms are decoded into ``term.Atom``. Pids and refs into ``term.Pid``
        and ``term.Reference`` respectively. Maps are decoded into Python
        ``dict``. Binaries and bit strings are decoded into ``term.Binary``
        object, with optional last bits omitted.

        :param options: See description on top of the module
        :param data: Bytes containing encoded term without 131 header
        :return: Tuple (Value, TailBytes) The function consumes as much data as
            possible and returns the tail. Tail can be used again to parse
            another term if there was any.
        :raises ETFDecodeException: on various errors or on an unsupported tag
    """
    tag = data[0]

    if tag in [TAG_ATOM_EXT, TAG_ATOM_UTF8_EXT]:
        len_data = len(data)
        if len_data < 3:
            return incomplete_data("decoding length for an atom name")

        len_expected = util.u16(data, 1) + 3
        if len_expected > len_data:
            return incomplete_data("decoding text for an atom")

        name = data[3:len_expected]
        enc = 'latin-1' if tag == TAG_ATOM_EXT else 'utf8'
        return _bytes_to_atom(name, enc, options), data[len_expected:]

    if tag == [TAG_SMALL_ATOM_EXT, TAG_SMALL_ATOM_UTF8_EXT]:
        len_data = len(data)
        if len_data < 2:
            return incomplete_data("decoding length for a small-atom name")

        len_expected = util.u16(data, 1) + 2
        name = data[2:len_expected]

        enc = 'latin-1' if tag == TAG_SMALL_ATOM_EXT else 'utf8'
        return _bytes_to_atom(name, enc, options), data[len_expected:]

    if tag == TAG_NIL_EXT:
        return [], data[1:]

    if tag == TAG_STRING_EXT:
        len_data = len(data)
        if len_data < 3:
            return incomplete_data("decoding length for a string")

        len_expected = util.u16(data, 1) + 3

        if len_expected > len_data:
            return incomplete_data()

        return data[3:len_expected].decode("utf8"), data[len_expected:]

    if tag == TAG_LIST_EXT:
        if len(data) < 5:
            return incomplete_data("decoding length for a list")
        len_expected = util.u32(data, 1)
        result = term.List()
        tail = data[5:]
        while len_expected > 0:
            term1, tail = binary_to_term_2(tail)
            result.append(term1)
            len_expected -= 1

        # Read list tail and set it
        list_tail, tail = binary_to_term_2(tail)
        result.set_tail(list_tail)

        return result, tail

    if tag == TAG_SMALL_TUPLE_EXT:
        if len(data) < 2:
            return incomplete_data("decoding length for a small tuple")
        len_expected = data[1]
        result = []
        tail = data[2:]
        while len_expected > 0:
            term1, tail = binary_to_term_2(tail)
            result.append(term1)
            len_expected -= 1

        return tuple(result), tail

    if tag == TAG_LARGE_TUPLE_EXT:
        if len(data) < 5:
            return incomplete_data("decoding length for a large tuple")
        len_expected = util.u32(data, 1)
        result = []
        tail = data[5:]
        while len_expected > 0:
            term1, tail = binary_to_term_2(tail)
            result.append(term1)
            len_expected -= 1

        return tuple(result), tail

    if tag == TAG_SMALL_INT:
        if len(data) < 2:
            return incomplete_data("decoding a 8-bit small uint")
        return data[1], data[2:]

    if tag == TAG_INT:
        if len(data) < 5:
            return incomplete_data("decoding a 32-bit int")
        return util.i32(data, 1), data[5:]

    if tag == TAG_PID_EXT:
        node, tail = binary_to_term_2(data[1:])
        id1 = util.u32(tail, 0)
        serial = util.u32(tail, 4)
        creation = tail[8]

        pid = term.Pid(node=node,
                       id=id1,
                       serial=serial,
                       creation=creation)
        return pid, tail[9:]

    if tag == TAG_NEW_REF_EXT:
        if len(data) < 2:
            return incomplete_data("decoding length for a new-ref")
        term_len = util.u16(data, 1)
        node, tail = binary_to_term_2(data[3:])
        creation = tail[0]
        id_len = 4 * term_len
        id1 = tail[1:id_len + 1]

        ref = term.Reference(node=node,
                             creation=creation,
                             id=id1)
        return ref, tail[id_len + 1:]

    if tag == TAG_MAP_EXT:
        if len(data) < 5:
            return incomplete_data("decoding length for a map")
        len_expected = util.u32(data, 1)
        result = {}
        tail = data[5:]
        while len_expected > 0:
            term1, tail = binary_to_term_2(tail)
            term2, tail = binary_to_term_2(tail)
            result[term1] = term2
            len_expected -= 1

        return result, tail

    if tag == TAG_BINARY_EXT:
        len_data = len(data)
        if len_data < 5:
            return incomplete_data("decoding length for a binary")
        len_expected = util.u32(data, 1) + 5
        if len_expected > len_data:
            return incomplete_data("decoding data for a binary")

        if options.get("binaries_as_bytes", False):
            return data[5:len_expected], data[len_expected:]

        bin1 = term.Binary(data=data[5:len_expected])
        return bin1, data[len_expected:]

    if tag == TAG_BIT_BINARY_EXT:
        len_data = len(data)
        if len_data < 6:
            return incomplete_data("decoding length for a bit-binary")
        len_expected = util.u32(data, 1) + 6
        lbb = data[5]
        if len_expected > len_data:
            return incomplete_data("decoding data for a bit-binary")

        if options.get("binaries_as_bytes", False):
            return data[6:len_expected], data[len_expected:]

        bin1 = term.Binary(data=data[6:len_expected],
                           last_byte_bits=lbb)
        return bin1, data[len_expected:]

    if tag == TAG_NEW_FLOAT_EXT:
        (result,) = struct.unpack(">d", data[1:9])
        return result, data[10:]

    if tag == TAG_NEW_FUN_EXT:
        # size = util.u32(data, 1)
        arity = data[5]
        uniq = data[6:22]
        index = util.u32(data, 22)
        num_free = util.u32(data, 26)
        (mod, tail) = binary_to_term_2(data[30:])
        (old_index, tail) = binary_to_term_2(tail)
        (old_uniq, tail) = binary_to_term_2(tail)
        (pid, tail) = binary_to_term_2(tail)

        free_vars = []
        while num_free > 0:
            (v, tail) = binary_to_term_2(tail)
            free_vars.append(v)
            num_free -= 1

        return term.Fun(mod=mod,
                        arity=arity,
                        pid=pid,
                        index=index,
                        uniq=uniq,
                        old_index=old_index,
                        old_uniq=old_uniq,
                        free=free_vars), tail

    raise ETFDecodeException("Unknown tag %d" % data[0])


def _pack_list(lst, tail):
    if len(lst) == 0:
        return bytes([TAG_NIL_EXT])

    data = b''
    for item in lst:
        data += term_to_binary_2(item)

    tail = term_to_binary_2(tail)
    return bytes([TAG_LIST_EXT]) + util.to_u32(len(lst)) + data + tail


def _pack_string(val):
    if len(val) == 0:
        return _pack_list([], [])

    # Save time here and don't check list elements to fit into a byte
    # Otherwise TODO: if all elements were bytes - we could use TAG_STRING_EXT

    return _pack_list(list(val), [])


def _pack_tuple(val):
    if len(val) < 256:
        data = bytes([TAG_SMALL_TUPLE_EXT, len(val)])
    else:
        data = bytes([TAG_LARGE_TUPLE_EXT]) + util.to_u32(len(val))

    for item in val:
        data += term_to_binary_2(item)

    return data


def _pack_dict(val: dict) -> bytes:
    data = bytes([TAG_MAP_EXT]) + util.to_u32(len(val))
    for k in val.keys():
        data += term_to_binary_2(k)
        data += term_to_binary_2(val[k])
    return data


def _pack_int(val):
    if 0 <= val < 256:
        return bytes([TAG_SMALL_INT, val])

    return bytes([TAG_INT]) + util.to_i32(val)


# TODO: maybe move this into atom class
def _pack_atom(text: str, encoding: str) -> bytes:
    # TODO: probably should be latin1 not utf8?
    return bytes([TAG_ATOM_EXT if encoding.startswith("latin")
                  else TAG_ATOM_UTF8_EXT]) \
           + util.to_u16(len(text)) \
           + bytes(text, encoding)


# TODO: maybe move this into pid class
def _pack_pid(val) -> bytes:
    data = bytes([TAG_PID_EXT]) + \
           term_to_binary_2(val.node_) + \
           util.to_u32(val.id_) + \
           util.to_u32(val.serial_) + \
           bytes([val.creation_])
    return data


# TODO: maybe move this into ref class
def _pack_ref(val) -> bytes:
    data = bytes([TAG_NEW_REF_EXT]) + util.to_u16(len(val.id_) // 4) + \
           term_to_binary_2(val.node_) + bytes([val.creation_]) + val.id_
    return data


def _serialize_object(obj):
    object_name = type(obj).__name__
    fields = {}
    for key in dir(obj):
        val = getattr(obj, key)
        if not callable(val) and not key.startswith("_"):
            fields[term.Atom(key)] = val

    return term.Atom(object_name), fields


def _pack_str(val):
    data = bytes([TAG_STRING_EXT]) + util.to_u16(len(val))
    data += bytes(val, "utf8")
    return data


def _pack_float(val):
    return bytes([TAG_NEW_FLOAT_EXT]) + struct.pack(">d", val)


def _pack_binary(data, last_byte_bits):
    if last_byte_bits == 8:
        return bytes([TAG_BINARY_EXT]) + util.to_u32(len(data)) + data

    return bytes([TAG_BIT_BINARY_EXT]) + util.to_u32(len(data)) + \
        bytes([last_byte_bits]) + data


def term_to_binary_2(val):
    """ Erlang lists are decoded into term.List object, whose ``elements_``
        field contains the data, ``tail_`` field has the optional tail and a
        helper function exists to assist with extracting an unicode string.

        :param val: Almost any Python value
        :return: bytes object with encoded data, but without a 131 header byte.
    """
    if type(val) == str:
        return _pack_str(val)

    elif type(val) == list:
        return _pack_list(val, [])

    elif isinstance(val, term.List):
        return _pack_list(val.elements_, val.tail_)

    elif type(val) == tuple:
        return _pack_tuple(val)

    elif type(val) == dict:
        return _pack_dict(val)

    elif type(val) == int:
        return _pack_int(val)

    elif type(val) == float:
        return _pack_float(val)

    elif val is None:
        return _pack_atom('undefined', 'latin-1')

    elif isinstance(val, term.Atom):
        return _pack_atom(val.text_, val.enc_)

    elif isinstance(val, term.Pid):
        return _pack_pid(val)

    elif isinstance(val, term.Reference):
        return _pack_ref(val)

    elif type(val) == bytes:
        return _pack_binary(val, 8)

    elif isinstance(val, term.Binary):
        return _pack_binary(val.bytes_, val.last_byte_bits_)

    return term_to_binary_2(_serialize_object(val))
    # obj_data = term_to_binary_2(_serialize_object(val))
    # print(util.hex_bytes(obj_data))
    # return obj_data
    # raise ETFEncodeException("Can't encode %s %s" % (type(val), str(val)))


def term_to_binary(val):
    """ Prepend the 131 header byte to encoded data.
    """
    return bytes([ETF_VERSION_TAG]) + term_to_binary_2(val)


__all__ = ['binary_to_term', 'binary_to_term_2',
           'term_to_binary', 'term_to_binary_2']
