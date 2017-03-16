from .base import Instruction


class PrintInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        print_value = chr(self.value) if self.flag else table[self.addresses[-1]].value
        print(print_value)
        return Instruction.execute(self, table)


class ReadInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        table[self.addresses[-1]].value = int(input())
        return Instruction.execute(self, table)
