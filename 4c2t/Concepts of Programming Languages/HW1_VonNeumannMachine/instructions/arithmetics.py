from .base import Instruction
from operator import add, sub, mul, floordiv, mod


class ArithmeticInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def calculate_result(self, operation, table):
        x = table[self.addresses[0]].value if self.flag % 2 == 0 else self.addresses[0]
        y = table[self.addresses[1]].value if self.flag // 2 == 0 else self.addresses[1]
        return operation(x, y)


class AddInstruction(ArithmeticInstruction):
    def __init__(self, flag, args):
        ArithmeticInstruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        res = self.calculate_result(add, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class SubInstruction(ArithmeticInstruction):
    def __init__(self, flag, args):
        ArithmeticInstruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        res = self.calculate_result(sub, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class MulInstruction(ArithmeticInstruction):
    def __init__(self, flag, args):
        ArithmeticInstruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        res = self.calculate_result(mul, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class DivInstruction(ArithmeticInstruction):
    def __init__(self, flag, args):
        ArithmeticInstruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        res = self.calculate_result(floordiv, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)


class ModInstruction(ArithmeticInstruction):
    def __init__(self, flag, args):
        ArithmeticInstruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        res = self.calculate_result(mod, table)
        table.set_arithmetic_result(res)
        return Instruction.execute(self, table)
