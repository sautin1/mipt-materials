from .enums import InstructionFlag

INSTRUCTION_LENGTH = 6


class Instruction(object):
    def __init__(self, flag=0, addresses=None, value=None):
        self.flag = flag
        self.addresses = addresses
        self.value = value

    def get_address_by_flag(self, table, is_first=False):
        result = None
        if is_first:
            address = self.addresses[0]
            if self.flag & InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR:
                result = table[table[address].value].value
            elif self.flag & InstructionFlag.FIRST_ARG_IS_ADDR:
                result = table[address].value
            else:
                result = address
        else:
            address = self.addresses[-1]
            if self.flag & InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR:
                result = table[table[address].value].value
            elif self.flag & InstructionFlag.LAST_ARG_IS_ADDR:
                result = table[address].value
            else:
                result = address
        return result

    def execute(self, table):
        table.inc_instruction_index()
        return False
