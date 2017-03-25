import numpy as np

from instructions.base import INSTRUCTION_LENGTH

from table import Table


class Machine(object):
    def __init__(self, path):
        table_bytes = np.fromfile(path, dtype=np.ubyte).reshape((-1, INSTRUCTION_LENGTH))
        self.table = Table(table_bytes)

    def run(self):
        found_stop = False
        while not found_stop:
            instruction_pointer = self.table.get_instruction_pointer()
            # print('run: ', instruction_pointer, self.table[instruction_pointer])
            instruction = self.table[instruction_pointer]
            found_stop = instruction.execute(self.table)
            # print('\tbytes:', Serializer.to_byte_array(instruction))

if __name__ == '__main__':
    input_file = 'translated/equal'
    machine = Machine(input_file)
    machine.run()
