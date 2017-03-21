from .arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from .io import ReadInstruction, PrintInstruction
from .memory import GlobInstruction, StackInstruction, PushInstruction, PopInstruction
from .other import JumpInstruction, CjumpInstruction, MoveInstruction, StopInstruction
from byte_utils import byte_array_to_int

from .enums import OpcodeType

opcode_type_to_instruction = {
    OpcodeType.GLOB: GlobInstruction,
    OpcodeType.STACK: StackInstruction,
    OpcodeType.PUSH: PushInstruction,
    OpcodeType.POP: PopInstruction,
    OpcodeType.MOVE: MoveInstruction,
    OpcodeType.JUMP: JumpInstruction,
    OpcodeType.CJUMP: CjumpInstruction,
    OpcodeType.ADD: AddInstruction,
    OpcodeType.SUB: SubInstruction,
    OpcodeType.MUL: MulInstruction,
    OpcodeType.DIV: DivInstruction,
    OpcodeType.MOD: ModInstruction,
    OpcodeType.PRINT: PrintInstruction,
    OpcodeType.READ: ReadInstruction,
    OpcodeType.STOP: StopInstruction
}


def instruction_from_bytes(byte_array):
    command, flag, args = byte_array[0], byte_array[1], byte_array[2:]
    value = byte_array_to_int(args)
    addresses = [byte_array_to_int(args[:2]), byte_array_to_int(args[2:])]
    return opcode_type_to_instruction[command](flag=flag, addresses=addresses, value=value)
