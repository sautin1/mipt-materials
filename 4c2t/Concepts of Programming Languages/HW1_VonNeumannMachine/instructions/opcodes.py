from enum import IntEnum


class OpcodeType(IntEnum):
    glob, stack, push, pop, move, jump, cjump, add, sub, mul, div, mod, print, read, stop = range(15)
