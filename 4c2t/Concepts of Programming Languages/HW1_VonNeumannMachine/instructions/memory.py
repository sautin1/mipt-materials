from .base import Instruction


class GlobInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=0):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        return Instruction.execute(self, table)


class StackInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=0):
        Instruction.__init__(self, flag, addresses, value)
        if args is None:
            self.value = value

    def to_bytes(self):
        pass

    def execute(self, table):
        return Instruction.execute(self, table)


class PushInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        push_value = self.value if self.flag else table[self.addresses[-1]].value
        table.push_to_stack(push_value)
        return Instruction.execute(self, table)


class PopInstruction(Instruction):
    def __init__(self, flag=0, addresses=None, value=None):
        Instruction.__init__(self, flag, addresses, value)

    def to_bytes(self):
        pass

    def execute(self, table):
        table.pop_from_stack()
        return Instruction.execute(self, table)
