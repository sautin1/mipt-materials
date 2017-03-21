from enum import IntEnum, IntFlag


class OpcodeType(IntEnum):
    GLOB, STACK, PUSH, POP, MOVE, JUMP, CJUMP, ADD, SUB, MUL, DIV, MOD, PRINT, READ, STOP = range(15)


class InstructionFlag(IntFlag):
    FIRST_ARG_IS_ADDR_OF_ADDR = 8
    FIRST_ARG_IS_ADDR = 4
    LAST_ARG_IS_ADDR_OF_ADDR = 2
    LAST_ARG_IS_ADDR = 1
    ARGS_ARE_VALUES = 0


class PrintEnum(IntEnum):
    NUMBER, ADDR, ADDR_OF_ADDR, CHAR = range(4)
