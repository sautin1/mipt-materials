from byte_utils import byte_array_to_int

INSTRUCTION_LENGTH = 6


class Instruction(object):
    def __init__(self, flag, args):
        self.flag = flag
        self.addresses = None
        self.value = None
        if args is not None:
            self.addresses = [byte_array_to_int(args[:2]), byte_array_to_int(args[2:])]
            self.value = byte_array_to_int(args)

    def execute(self, table):
        table.inc_instruction_index()
        return False
