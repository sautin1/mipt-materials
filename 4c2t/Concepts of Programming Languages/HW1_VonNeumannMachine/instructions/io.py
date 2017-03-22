from .base import Instruction

from instructions.enums import OpcodeType, PrintEnum, InstructionFlag


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
        elif self.flag == PrintEnum.ADDR:
            value = table[self.addresses[-1]].value
        elif self.flag == PrintEnum.ADDR_OF_ADDR:
            value = table[table[self.addresses[-1]].value].value

        print(value)
        return Instruction.execute(self, table)


class ReadInstruction(Instruction):
    def __init__(self, flag=InstructionFlag.LAST_ARG_IS_ADDR, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.READ

    def execute(self, table):
        is_address = self.flag & (InstructionFlag.LAST_ARG_IS_ADDR | InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR)
        assert is_address, 'wrong flag'
        idx = self.addresses[-1] if self.flag == InstructionFlag.LAST_ARG_IS_ADDR else table[self.addresses[-1]].value
        table[idx].value = int(input())
        return Instruction.execute(self, table)
