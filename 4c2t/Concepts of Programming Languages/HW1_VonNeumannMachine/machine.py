import numpy as np

from instructions.base import INSTRUCTION_LENGTH
from instructions.enums import OpcodeType, InstructionFlag, PrintEnum

from table import Table
from assembler import Serializer


class Machine(object):
    def __init__(self, path):
        table_bytes = np.fromfile(path, dtype=np.ubyte).reshape((-1, INSTRUCTION_LENGTH))
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
    G0 = 1
    HAS_ELSE_BRANCH = 16
    x = np.array([OpcodeType.GLOB, InstructionFlag.ARGS_ARE_VALUES, 0, 0, 0, 0,  # 0
                  OpcodeType.GLOB, InstructionFlag.ARGS_ARE_VALUES, 0, 0, 0, 0,  # 6
                  OpcodeType.GLOB, InstructionFlag.ARGS_ARE_VALUES, 0, 0, 0, 0,  # 12
                  OpcodeType.MOVE, InstructionFlag.FIRST_ARG_IS_ADDR, 0, 12, 0, 4,  # 18
                  OpcodeType.SUB, InstructionFlag.FIRST_ARG_IS_ADDR, 0, 12, 0, 1,  # 24
                  OpcodeType.PRINT, PrintEnum.ADDR, 0, 0, 0, 12,  # 30
                  OpcodeType.CJUMP, G0 | HAS_ELSE_BRANCH, 0, 48, 0, 24,  # 36
                  OpcodeType.PRINT, PrintEnum.ADDR, 0, 0, 0, 0,  # 42
                  OpcodeType.STOP, InstructionFlag.ARGS_ARE_VALUES, 0, 0, 0, 0  # 48
                  ], dtype=np.ubyte)
    arr = x.tobytes()
    with open('data/fib', 'wb') as fout:
        fout.write(arr)
    machine = Machine('data/fib')
    machine.run()
