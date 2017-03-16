from enum import IntEnum

from .arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from .io import ReadInstruction, PrintInstruction
from .memory import GlobInstruction, StackInstruction, PushInstruction, PopInstruction
from .other import JmpInstruction, CjmpInstruction, MoveInstruction, StopInstruction
from byte_utils import byte_array_to_int


class OpcodeType(IntEnum):
    glob, stack, push, pop, mov, jmp, cjump, add, sub, mul, div, mod, print, read, stop = range(15)


opcode_type_to_instruction = {
    OpcodeType.glob: GlobInstruction,
    OpcodeType.stack: StackInstruction,
    OpcodeType.push: PushInstruction,
    OpcodeType.pop: PopInstruction,
    OpcodeType.mov: MoveInstruction,
    OpcodeType.jmp: JmpInstruction,
    OpcodeType.cjump: CjmpInstruction,
    OpcodeType.add: AddInstruction,
    OpcodeType.sub: SubInstruction,
    OpcodeType.mul: MulInstruction,
    OpcodeType.div: DivInstruction,
    OpcodeType.mod: ModInstruction,
    OpcodeType.print: PrintInstruction,
    OpcodeType.read: ReadInstruction,
    OpcodeType.stop: StopInstruction
}


def instruction_from_bytes(byte_array):
    command, flag, args = byte_array[0], byte_array[1], byte_array[2:]
    value = byte_array_to_int(args)
    addresses = [byte_array_to_int(args[:2]), byte_array_to_int(args[2:])]
    return opcode_type_to_instruction[command](flag=flag, addresses=addresses, value=value)
