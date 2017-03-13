from enum import IntEnum

from .arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from .io import ReadInstruction, PrintInstruction
from .memory import GlobInstruction, StackInstruction, PushInstruction, PopInstruction
from .other import JmpInstruction, CjmpInstruction, MovInstruction, StopInstruction


class CommandType(IntEnum):
    glob, stack, push, pop, mov, jmp, cjmp, add, sub, mul, div, mod, print, read, stop = range(15)


command_type_to_instruction = {
    CommandType.glob: GlobInstruction,
    CommandType.stack: StackInstruction,
    CommandType.push: PushInstruction,
    CommandType.pop: PopInstruction,
    CommandType.mov: MovInstruction,
    CommandType.jmp: JmpInstruction,
    CommandType.cjmp: CjmpInstruction,
    CommandType.add: AddInstruction,
    CommandType.sub: SubInstruction,
    CommandType.mul: MulInstruction,
    CommandType.div: DivInstruction,
    CommandType.mod: ModInstruction,
    CommandType.print: PrintInstruction,
    CommandType.read: ReadInstruction,
    CommandType.stop: StopInstruction
}


def instruction_from_bytes(byte_array):
    command, flag, args = byte_array[0], byte_array[1], byte_array[2:]
    return command_type_to_instruction[command](flag, args)
