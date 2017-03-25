from instructions.enums import OpcodeType
from instructions.construction import instruction_from_bytes
from instructions.base import INSTRUCTION_LENGTH

from instructions.memory import GlobInstruction, StackInstruction


class Table(object):
    def __init__(self, table_bytes=None):
        self.stack = []
        self.instructions = []
        if table_bytes is not None:
            stack_instruction_index = next((i for i, x in enumerate(table_bytes) if x[0] == OpcodeType.STACK),
                                           len(table_bytes))
            self.instructions = list(map(instruction_from_bytes, table_bytes))
            self.stack = self.instructions[stack_instruction_index:]
            self.instructions = self.instructions[:stack_instruction_index]
        else:
            self.instructions += [
                GlobInstruction(),  # instruction pointer
                GlobInstruction(),  # stack pointer
                GlobInstruction(),  # arithmetic result
                GlobInstruction(),  # temporary glob
            ]
            self.stack = []

    def __getitem__(self, item):
        assert item % INSTRUCTION_LENGTH == 0, 'Table.__get_item__: index is wrong'
        idx = item // INSTRUCTION_LENGTH
        if idx < len(self.instructions):
            result = self.instructions[idx]
        else:
            idx -= len(self.instructions)
            result = self.stack[idx]
        return result

    @staticmethod
    def get_arithmetic_glob_address():
        return INSTRUCTION_LENGTH * 2

    @staticmethod
    def get_temporary_glob_address():
        return INSTRUCTION_LENGTH * 3

    @staticmethod
    def get_stack_pointer_address():
        return INSTRUCTION_LENGTH

    def __update_stack_pointer(self):
        self.instructions[1].value = INSTRUCTION_LENGTH * (len(self.stack) - 1 + len(self.instructions))

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
        self.__update_stack_pointer()
        return self.instructions[1].value

    def pop_from_stack(self):
        self.stack.pop()
        self.__update_stack_pointer()

    def get_arithmetic_result(self):
        return self.instructions[2].value

    def set_arithmetic_result(self, new_value):
        self.instructions[2].value = new_value

    def size(self, need_stack=True):
        result = len(self.instructions) * INSTRUCTION_LENGTH
        if need_stack:
            result += len(self.stack) * INSTRUCTION_LENGTH
        return result

    def get_table_with_stack(self):
        return self.instructions + self.stack
