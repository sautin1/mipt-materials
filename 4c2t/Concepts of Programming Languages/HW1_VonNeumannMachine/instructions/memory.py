from .base import Instruction


class GlobInstruction(Instruction):
    def __init__(self, flag=None, args=None):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        return Instruction.execute(self, table)


class StackInstruction(Instruction):
    def __init__(self, flag=None, args=None, value=None):
        Instruction.__init__(self, flag, args)
        if args is None:
            self.value = value

    def to_bytes(self):
        pass

    def execute(self, table):
        return Instruction.execute(self, table)


class PushInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        push_value = self.value if self.flag else table[self.addresses[-1]].value
        table.push_to_stack(push_value)
        return Instruction.execute(self, table)


class PopInstruction(Instruction):
    def __init__(self, flag, args):
        Instruction.__init__(self, flag, args)

    def to_bytes(self):
        pass

    def execute(self, table):
        table.pop_from_stack()
        return Instruction.execute(self, table)