from .base import Instruction


class MoveInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        address_to, address_from = self.addresses
        table[address_to].value = address_from if self.flag else table[address_from].value
        return Instruction.execute(self, table)


class JmpInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        table.set_instruction_pointer(self.addresses[-1])
        return False


class CjmpInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        if table.get_arithmetic_result():
            table.set_instruction_pointer(self.addresses[-1])
        elif self.flag:
            table.set_instruction_pointer(self.addresses[0])
        else:
            Instruction.execute(self, table)
        return False

class StopInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        return True
