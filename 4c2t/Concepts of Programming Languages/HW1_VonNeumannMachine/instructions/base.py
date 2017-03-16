INSTRUCTION_LENGTH = 6


class Instruction(object):
    def __init__(self, flag=0, addresses=None, value=None):
        self.flag = flag
        self.addresses = addresses
        self.value = value

    def execute(self, table):
        table.inc_instruction_index()
        return False
