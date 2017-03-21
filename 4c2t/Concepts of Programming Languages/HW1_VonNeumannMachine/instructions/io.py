from .base import Instruction

from instructions.enums import OpcodeType, PrintEnum


class PrintInstruction(Instruction):
    def __init__(self, flag=PrintEnum.NUMBER, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.PRINT

    def execute(self, table):
        value = self.value
        if self.flag == PrintEnum.CHAR:
            value = chr(self.value)
        elif self.flag == PrintEnum.ADDR_OF_ADDR:
            value = table[table[self.addresses[-1]].value].value
        elif self.flag == PrintEnum.ADDR:
            value = table[self.addresses[-1]].value

        print(value)
        return Instruction.execute(self, table)


class ReadInstruction(Instruction):
    def __init__(self, flag=PrintEnum.ADDR, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.READ

    def execute(self, table):
        idx = self.addresses[-1] if self.flag == PrintEnum.ADDR else table[self.addresses[-1]].value
        table[idx].value = int(input())
        return Instruction.execute(self, table)
