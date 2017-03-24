from .base import Instruction

from instructions.enums import OpcodeType, InstructionFlag


class GlobInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=0):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.GLOB

    def execute(self, table):
        return Instruction.execute(self, table)


class StackInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=0):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.STACK

    def execute(self, table):
        return Instruction.execute(self, table)


class PushInstruction(Instruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.PUSH

    def execute(self, table):
        push_value = self.get_value_by_flag(table, need_address=False)
        table.push_to_stack(push_value)
        return Instruction.execute(self, table)


class PopInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.POP

    def execute(self, table):
        table.pop_from_stack()
        return Instruction.execute(self, table)
