from .base import Instruction
from operator import add, sub, mul, floordiv, mod

from .enums import OpcodeType, InstructionFlag


class ArithmeticInstruction(Instruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def calculate_result(self, operation, table):
        x = self.get_value_by_flag(table, is_first=True, need_address=True)
        y = self.get_value_by_flag(table, is_first=False, need_address=True)
        return operation(x, y)


class AddInstruction(ArithmeticInstruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.ADD

    def execute(self, table):
        res = self.calculate_result(add, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class SubInstruction(ArithmeticInstruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.SUB

    def execute(self, table):
        res = self.calculate_result(sub, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class MulInstruction(ArithmeticInstruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.MUL

    def execute(self, table):
        res = self.calculate_result(mul, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class DivInstruction(ArithmeticInstruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.DIV

    def execute(self, table):
        res = self.calculate_result(floordiv, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class ModInstruction(ArithmeticInstruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.MOD

    def execute(self, table):
        res = self.calculate_result(mod, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)
