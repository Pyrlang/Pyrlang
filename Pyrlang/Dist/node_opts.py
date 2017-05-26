#
# Distribution flags from http://erlang.org/doc/apps/erts/erl_dist_protocol.html
#
DFLAG_PUBLISHED = 0x01
DFLAG_ATOM_CACHE = 0x02
DFLAG_EXT_REFS = 0x04
DFLAG_DIST_MONITOR = 0x08
DFLAG_FUN_TAGS = 0x10
DFLAG_DIST_MONITOR_NAME = 0x20
DFLAG_HIDDEN_ATOM_CACHE = 0x40
DFLAG_NEW_FUN_TAGS = 0x80
DFLAG_EXT_PIDS_PORTS = 0x100
DFLAG_EXPORT_PTR_TAG = 0x200
DFLAG_BIT_BINARIES = 0x400
DFLAG_NEW_FLOATS = 0x800
DFLAG_UNICODE_IO = 0x1000
DFLAG_DIST_HDR_ATOM_CACHE = 0x2000
DFLAG_SMALL_ATOM_TAGS = 0x4000
# 0x8000
DFLAG_UTF8_ATOMS = 0x10000
DFLAG_MAP_TAG = 0x20000
DFLAG_BIG_CREATION = 0x40000

DEFAULT_DFLAGS = (DFLAG_EXT_REFS |
                  DFLAG_EXT_PIDS_PORTS |
                  DFLAG_FUN_TAGS | DFLAG_NEW_FUN_TAGS |
                  DFLAG_EXPORT_PTR_TAG |
                  DFLAG_BIT_BINARIES |
                  DFLAG_NEW_FLOATS |
                  DFLAG_MAP_TAG |
                  DFLAG_DIST_MONITOR_NAME | DFLAG_DIST_MONITOR)


class NodeOpts:
    def __init__(self, cookie: str, dflags: int = DEFAULT_DFLAGS) -> None:
        self.dflags_ = dflags
        self.cookie_ = cookie
        self.network_tick_time_ = 60
