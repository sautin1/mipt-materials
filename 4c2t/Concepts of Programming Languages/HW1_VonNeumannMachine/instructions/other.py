from enum import IntEnum

from .base import Instruction
from .enums import OpcodeType, InstructionFlag


class CjumpLogicalOperator(IntEnum):
    EQ0, G0, L0, GE0, LE0, NEQ0 = range(6)
    # number of elements cannot exceed CjumpInstruction.get_else_bitwise_shift()


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
    def __init__(self, flag=InstructionFlag.FIRST_ARG_IS_ADDR, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.MOVE

    def execute(self, table):
        is_first_addr = self.flag & (InstructionFlag.FIRST_ARG_IS_ADDR | InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR)
        assert is_first_addr, 'Move target has to be an address'
        idx = self.addresses[0]
        if self.flag & InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR:
            idx = table[self.addresses[0]].value
        value = self.get_value_by_flag(table, is_first=False, need_address=True)

        table[idx].value = value
        return Instruction.execute(self, table)


class JumpInstruction(Instruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_type():
        return OpcodeType.JUMP

    def execute(self, table):
        address = self.get_value_by_flag(table, is_first=False, need_address=True)
        table.set_instruction_pointer(address)
        return False


class CjumpInstruction(Instruction):
    def __init__(self, flag=InstructionFlag.ARGS_ARE_VALUES, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    @staticmethod
    def get_else_bitwise_shift():
        return 16

    @staticmethod
    def add_else_to_flag(flag):
        return flag | CjumpInstruction.get_else_bitwise_shift()

    @staticmethod
    def get_type():
        return OpcodeType.CJUMP

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
        return OpcodeType.STOP

    def execute(self, table):
        return True
