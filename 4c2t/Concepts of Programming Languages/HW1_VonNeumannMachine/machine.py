import numpy as np

from instructions.base import INSTRUCTION_LENGTH
from instructions.construction import OpcodeType, instruction_from_bytes
from instructions.memory import StackInstruction

from instructions.memory import GlobInstruction


class Table(object):
    def __init__(self, table_bytes=None):
        self.stack = []
        self.instructions = []
        if table_bytes is not None:
            stack_instruction_index = next((i for i, x in enumerate(table_bytes) if x[0] == OpcodeType.stack),
                                           len(table_bytes))
            self.instructions = list(map(instruction_from_bytes, table_bytes))
            self.stack = self.instructions[stack_instruction_index:]
            self.instructions = self.instructions[:stack_instruction_index]
        else:
            self.instructions += [
                GlobInstruction(),  # instruction pointer
                GlobInstruction(),  # stack pointer
                GlobInstruction()  # arithmetic result
            ]
            self.stack = []

    def __getitem__(self, item):
        return self.instructions[item // INSTRUCTION_LENGTH]

    @staticmethod
    def get_arithmetic_result_address():
        return INSTRUCTION_LENGTH * 2

    def get_instruction_index(self):
        return self.instructions[0].value // INSTRUCTION_LENGTH

    def set_instruction_index(self, new_value):
        self.set_instruction_pointer(new_value * INSTRUCTION_LENGTH)

    def inc_instruction_index(self):
        self.set_instruction_index(self.get_instruction_index() + 1)

    def set_instruction_pointer(self, new_value):
        self.instructions[0].value = new_value

    def get_instruction_pointer(self):
        return self.instructions[0].value

    def push_to_stack(self, value=0):
        self.stack.append(StackInstruction(value=value))
        self.instructions[1].value += INSTRUCTION_LENGTH
        return self.instructions[1].value

    def pop_from_stack(self):
        self.stack.pop()
        self.instructions[1].value -= INSTRUCTION_LENGTH

    def get_arithmetic_result(self):
        return self.instructions[2].value

    def set_arithmetic_result(self, new_value):
        self.instructions[2].value = new_value

    def size_without_stack(self):
        return len(self.instructions) * INSTRUCTION_LENGTH

    def get_table_with_stack(self):
        return self.instructions + self.stack


class Machine(object):
    def __init__(self, path):
        table_bytes = np.fromfile(path, dtype=np.byte).reshape((-1, INSTRUCTION_LENGTH))
        self.table = Table(table_bytes)

    def run(self):
        found_stop = False
        while not found_stop:
            instruction_pointer = self.table.get_instruction_pointer()
            print('run: ', instruction_pointer, self.table[instruction_pointer])
            instruction = self.table[instruction_pointer]
            found_stop = instruction.execute(self.table)

if __name__ == '__main__':
    x = np.array([OpcodeType.glob, 0, 0, 0, 0, 0,  # 0
                  OpcodeType.glob, 0, 0, 0, 0, 0,  # 6
                  OpcodeType.glob, 0, 0, 0, 0, 0,  # 12
                  OpcodeType.mov, 1, 0, 12, 0, 4,  # 18
                  OpcodeType.sub, 2, 0, 12, 0, 1,  # 24
                  OpcodeType.print, 0, 0, 0, 0, 12,  # 30
                  OpcodeType.cjump, 1 | 32, 0, 48, 0, 24,  # 36
                  OpcodeType.print, 0, 0, 0, 0, 0,  # 42
                  OpcodeType.stop, 0, 0, 0, 0, 0  # 48
                  ], dtype=np.byte)
    arr = x.tobytes()
    with open('data/fib', 'wb') as fout:
        fout.write(arr)
    machine = Machine('data/fib')
    machine.run()
