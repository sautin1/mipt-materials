from enum import IntEnum
import numpy as np

from .base import Instruction
from .opcodes import OpcodeType

from byte_utils import int_to_byte_array


class CjumpLogicalOperator(IntEnum):
    EQ0, G0, L0, GE0, LE0, NEQ0 = range(6)


logical_operator_name_to_logical_operator = {
    'EQ0': CjumpLogicalOperator.EQ0,
    'G0': CjumpLogicalOperator.G0,
    'L0': CjumpLogicalOperator.L0,
    'GE0': CjumpLogicalOperator.GE0,
    'LE0': CjumpLogicalOperator.LE0,
    'NEQ0': CjumpLogicalOperator.NEQ0
}


logical_operator_to_predicate = {
    CjumpLogicalOperator.EQ0: lambda x: x == 0,
    CjumpLogicalOperator.G0: lambda x: x > 0,
    CjumpLogicalOperator.L0: lambda x: x < 0,
    CjumpLogicalOperator.GE0: lambda x: x >= 0,
    CjumpLogicalOperator.LE0: lambda x: x <= 0,
    CjumpLogicalOperator.NEQ0: lambda x: x != 0
}


class MoveInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.move

    def execute(self, table):
        address_to, address_from = self.addresses
        table[address_to].value = address_from if self.flag else table[address_from].value
        return Instruction.execute(self, table)


class JumpInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.jump

    def execute(self, table):
        table.set_instruction_pointer(self.addresses[-1])
        return False


class CjumpInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_else_bitwise_shift():
        return 32

    @staticmethod
    def add_else_to_flag(flag):
        return flag | CjumpInstruction.get_else_bitwise_shift()

    @staticmethod
    def get_type():
        return OpcodeType.cjump

    def execute(self, table):
        flag = self.flag & (CjumpInstruction.get_else_bitwise_shift() - 1)
        predicate = logical_operator_to_predicate[CjumpLogicalOperator(flag)]
        need_else = self.flag & CjumpInstruction.get_else_bitwise_shift()
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

    @staticmethod
    def get_type():
        return OpcodeType.cjump

    def execute(self, table):
        return True
