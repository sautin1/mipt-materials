import numpy as np

from instructions import INSTRUCTION_LENGTH, CommandType, instruction_from_bytes


class Machine(object):
    def __init__(self):
        self.byte_matrix = None

    def load_table(self, path):
        self.byte_matrix = np.fromfile(path, dtype=np.byte).reshape((-1, INSTRUCTION_LENGTH))

    def instruction_pointer(self):
        return self.byte_matrix[0][-1]

    def run(self):
        found_stop = False
        while not found_stop:
            instruction_idx = self.instruction_pointer() // INSTRUCTION_LENGTH
            instruction = instruction_from_bytes(self.byte_matrix[instruction_idx])
            found_stop = instruction.execute(self.byte_matrix)


if __name__ == '__main__':
    x = np.array([CommandType.glob, 0, 0, 0, 0, 0,  # ip
                  CommandType.glob, 0, 0, 0, 0, 0,  # sp
                  CommandType.glob, 0, 0, 0, 10, 12,
                  CommandType.print, 0, 0, 0, 0, 1,
                  CommandType.print, 1, 0, 0, 0, 65,
                  CommandType.stop, 0, 0, 0, 0, 0], dtype=np.byte)
    arr = x.tobytes()
    with open('data/fib', 'wb') as fout:
        fout.write(arr)
    machine = Machine()
    machine.load_table('data/fib')
    machine.run()
