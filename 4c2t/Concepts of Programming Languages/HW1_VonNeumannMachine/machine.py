import numpy as np

from instructions.base import INSTRUCTION_LENGTH
from instructions.opcodes import OpcodeType

from table import Table
from assembler import Serializer


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
            print('\tbytes:', Serializer.to_byte_array(instruction))

if __name__ == '__main__':
    x = np.array([OpcodeType.glob, 0, 0, 0, 0, 0,  # 0
                  OpcodeType.glob, 0, 0, 0, 0, 0,  # 6
                  OpcodeType.glob, 0, 0, 0, 0, 0,  # 12
                  OpcodeType.move, 1, 0, 12, 0, 4,  # 18
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
