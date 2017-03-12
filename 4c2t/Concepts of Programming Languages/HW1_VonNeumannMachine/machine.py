import numpy as np

from instructions.base import INSTRUCTION_LENGTH
from instructions.construction import CommandType, instruction_from_bytes
from instructions.memory import StackInstruction


class Table(object):
    def __init__(self, table_bytes):
        self.instructions = list(map(instruction_from_bytes, table_bytes))

    def __getitem__(self, item):
        return self.instructions[item // INSTRUCTION_LENGTH]

    def get_instruction_index(self):
        return self.instructions[0].value // INSTRUCTION_LENGTH

    def set_instruction_index(self, new_value):
        self.set_instruction_pointer(new_value * INSTRUCTION_LENGTH)

    def inc_instruction_index(self):
        self.set_instruction_index(self.get_instruction_index() + 1)

    def set_instruction_pointer(self, new_value):
        self.instructions[0].value = new_value

    def push_to_stack(self, value):
        self.instructions.append(StackInstruction(value=value))
        self.instructions[1].value += INSTRUCTION_LENGTH

    def pop_from_stack(self):
        self.instructions.pop()
        self.instructions[1].value -= INSTRUCTION_LENGTH

    def set_arithmetic_result(self, new_value):
        self.instructions[2].value = new_value


class Machine(object):
    def __init__(self, path):
        table_bytes = np.fromfile(path, dtype=np.byte).reshape((-1, INSTRUCTION_LENGTH))
        self.table = Table(table_bytes)

    def run(self):
        table = self.table
        found_stop = False
        while not found_stop:
            instruction_idx = table.get_instruction_index()
            print('run: ', table[instruction_idx].value)
            instruction = table.instructions[instruction_idx]
            found_stop = instruction.execute(table)

if __name__ == '__main__':
    # x = np.array([CommandType.glob, 0, 0, 0, 0, 0,  # ip
    #               CommandType.glob, 0, 0, 0, 0, 0,  # sp
    #               CommandType.glob, 0, 0, 0, 0, 0,  # arithmetic_result
    #               CommandType.glob, 0, 0, 0, 10, 12,
    #               CommandType.print, 0, 0, 0, 0, 1,
    #               CommandType.print, 1, 0, 0, 0, 65,
    #               CommandType.stop, 0, 0, 0, 0, 0], dtype=np.byte)
    x = np.array([CommandType.glob, 0, 0, 0, 0, 0,
                  CommandType.glob, 0, 0, 0, 0, 0,
                  CommandType.glob, 0, 0, 0, 0, 0,
                  CommandType.mov, 0, 0, 2, 0, 0,
                  CommandType.print, 0, 0, 0, 0, 2,
                  CommandType.stop, 0, 0, 0, 0, 0], dtype=np.byte)
    arr = x.tobytes()
    with open('data/fib', 'wb') as fout:
        fout.write(arr)
    machine = Machine('data/fib')
    machine.run()
