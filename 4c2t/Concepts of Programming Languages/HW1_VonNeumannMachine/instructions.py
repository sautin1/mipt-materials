from enum import IntEnum
import numpy as np

INSTRUCTION_LENGTH = 6


def increase_instruction_pointer(table, increment):
    table[0][-1] += increment


def byte_array_to_int(byte_array, dtype='>i4'):
    return np.fromstring(byte_array.tostring(), dtype=dtype)[0]


def byte_array_to_value(byte_array):
    return byte_array_to_int(byte_array)  # big-endian np.int32


def byte_array_to_address(byte_array):
    return byte_array_to_int(byte_array, dtype='>i2')  # big-endian np.int16


class CommandType(IntEnum):
    glob, stack, mov, jmp, add, sub, mul, div, mod, print, read, stop = range(12)


class Instruction(object):
    def __init__(self, flag, value):
        self.flag = flag
        self.value = value

    def execute(self, table):
        raise NotImplementedError


class GlobInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        return np.array([CommandType.glob, self.flag, self.value], dtype=np.byte)

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class StackInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class MovInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        address_to, address_from = byte_array_to_address(self.value[:2]), byte_array_to_address(self.value[2:])
        table[address_to] = address_from
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class JmpInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class AddInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class SubInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class MulInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class DivInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class ModInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class PrintInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        return np.array([CommandType.print, self.flag, self.value], dtype=np.byte)

    def execute(self, table):
        if self.flag > 0:
            print(chr(byte_array_to_value(self.value)))
        else:
            address = byte_array_to_address(self.value[-2:])
            print(table[address][-1])
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class ReadInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        increase_instruction_pointer(table, INSTRUCTION_LENGTH)
        return False


class StopInstruction(Instruction):
    def __init__(self, flag, value):
        Instruction.__init__(self, flag, value)

    def to_bytes(self):
        return np.array([CommandType.print, self.flag, self.value], dtype=np.byte)

    def execute(self, table):
        return True


command_type_to_instruction = {
    CommandType.glob: GlobInstruction,
    CommandType.stack: StackInstruction,
    CommandType.mov: MovInstruction,
    CommandType.jmp: JmpInstruction,
    CommandType.add: AddInstruction,
    CommandType.sub: SubInstruction,
    CommandType.mul: MulInstruction,
    CommandType.div: DivInstruction,
    CommandType.mod: ModInstruction,
    CommandType.print: PrintInstruction,
    CommandType.read: ReadInstruction,
    CommandType.st: StopInstruction
}


def split_byte_array(byte_array):
    return byte_array[0], byte_array[1], byte_array[2:]


def instruction_from_bytes(byte_array):
    command, flag, value = split_byte_array(byte_array)
    return command_type_to_instruction[command](flag, value)


def instruction_to_bytes(instruction):
    return instruction.to_bytes()

if __name__ == '__main__':
    x = np.array([CommandType.print, 0, 0, 0, 1, 2], dtype=np.byte)
    x.tofile('data/fib')
    arr = np.fromfile('data/fib', dtype=np.byte)[: INSTRUCTION_LENGTH]
    print(byte_array_to_value(arr[2:]))
    print(byte_array_to_address(arr[4:]))