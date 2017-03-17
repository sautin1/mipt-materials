from .base import Instruction

from instructions.opcodes import OpcodeType


class PrintInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.print

    def execute(self, table):
        print_value = chr(self.value) if self.flag else table[self.addresses[-1]].value
        print(print_value)
        return Instruction.execute(self, table)


class ReadInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.read

    def execute(self, table):
        table[self.addresses[-1]].value = int(input())
        return Instruction.execute(self, table)
