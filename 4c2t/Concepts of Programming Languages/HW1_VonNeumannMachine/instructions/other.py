from enum import IntEnum
from .base import Instruction


class CjmpLogicalOperator(IntEnum):
    EQ0, G0, L0, GE0, LE0, NEQ0 = range(6)


logical_operator_name_to_logical_operator = {
    'EQ0': CjmpLogicalOperator.EQ0,
    'G0': CjmpLogicalOperator.G0,
    'L0': CjmpLogicalOperator.L0,
    'GE0': CjmpLogicalOperator.GE0,
    'LE0': CjmpLogicalOperator.LE0,
    'NEQ0': CjmpLogicalOperator.NEQ0
}


logical_operator_to_predicate = {
    CjmpLogicalOperator.EQ0: lambda x: x == 0,
    CjmpLogicalOperator.G0: lambda x: x > 0,
    CjmpLogicalOperator.L0: lambda x: x < 0,
    CjmpLogicalOperator.GE0: lambda x: x >= 0,
    CjmpLogicalOperator.LE0: lambda x: x <= 0,
    CjmpLogicalOperator.NEQ0: lambda x: x != 0
}


class MoveInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        address_to, address_from = self.addresses
        table[address_to].value = address_from if self.flag else table[address_from].value
        return Instruction.execute(self, table)


class JmpInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        table.set_instruction_pointer(self.addresses[-1])
        return False


class CjmpInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_else_bitwise_shift():
        return 32

    @staticmethod
    def add_else_to_flag(flag):
        return flag | CjmpInstruction.get_else_bitwise_shift()

    def to_bytes(self):
        pass

    def execute(self, table):
        flag = self.flag & (CjmpInstruction.get_else_bitwise_shift() - 1)
        predicate = logical_operator_to_predicate[CjmpLogicalOperator(flag)]
        need_else = self.flag & CjmpInstruction.get_else_bitwise_shift()
        if predicate(table.get_arithmetic_result()):
            table.set_instruction_pointer(self.addresses[-1])
        elif need_else:
            table.set_instruction_pointer(self.addresses[0])
        else:
            Instruction.execute(self, table)
        return False


class StopInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        return True
