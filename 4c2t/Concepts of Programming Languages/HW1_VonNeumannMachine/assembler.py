import shlex
import numpy as np

from table import Table

from instructions.memory import GlobInstruction
from instructions.arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from instructions.io import PrintInstruction, ReadInstruction
from instructions.other import MoveInstruction, JumpInstruction, CjumpInstruction
from instructions.other import logical_operator_name_to_logical_operator

from byte_utils import int_to_byte_array

command_name_to_arithmetic_instruction = {
    'ADD': AddInstruction,
    'SUB': SubInstruction,
    'MUL': MulInstruction,
    'DIV': DivInstruction,
    'MOD': ModInstruction
}


class Serializer(object):
    @staticmethod
    def to_byte_array(instruction, need_addresses=True):
        result = np.array([instruction.get_type(), instruction.flag])
        if need_addresses:
            addresses = np.array(list(map(lambda address: int_to_byte_array(address, 2),
                                          instruction.addresses))).flatten()
            result = np.concatenate((result, addresses))
        else:
            value = int_to_byte_array(instruction.value, 4)
            result = np.append(result, value)
        return result

    @staticmethod
    def to_byte_string(instruction, need_addresses=True):
        return Serializer.to_byte_array(instruction, need_addresses).tobytes()


class Assembler(object):
    def __init__(self, path_from):
        with open(path_from, 'r') as fin:
            lines = map(str.strip, fin.readlines())
        self.words = list(map(shlex.split, lines))
        self.table = Table()

        self.label_name_to_number = {}
        self.label_number_to_address = {}
        self.instructions_with_wrong_addresses = []

        self.global_name_to_address = {}
        self.local_name_to_address_stack = []
        self.arg_name_to_address_stack = []

        # self.args_passed_addresses = []
        self.function_to_argument_names = {}
        self.function_to_local_names = {}

        self.command_name_to_parser = {
            'GLOB': self.parse_glob,
            'VAR': self.parse_var,

            'PRINT': self.parse_print,
            'READ': self.parse_read,

            'MOVE': self.parse_move,
            'LABEL': self.parse_label,
            'JUMP': self.parse_jump,
            'IF': self.parse_if,

            'FUN': self.parse_fun,
            'CALL': self.parse_call,
            'RET': self.parse_ret,

            'ADD': self.parse_arithmetic,
            'SUB': self.parse_arithmetic,
            'MUL': self.parse_arithmetic,
            'DIV': self.parse_arithmetic,
            'MOD': self.parse_arithmetic
        }

    def __get_var_address(self, name):
        result = self.local_name_to_address_stack[-1].get(name) or self.arg_name_to_address_stack[-1].get(name)
        return result or self.global_name_to_address.get(name)

    def __get_label_number_or_add(self, label_name):
        label_number = len(self.label_name_to_number)
        return self.label_name_to_number.setdefault(label_name, label_number)

    def __replace_label_numbers(self):
        for idx, need_replace_left, need_replace_right in self.instructions_with_wrong_addresses:
            if need_replace_left:
                label_number = self.table.instructions[idx].addresses[0]
                self.table.instructions[idx].addresses[0] = self.label_number_to_address[label_number]
            if need_replace_right:
                label_number = self.table.instructions[idx].addresses[1]
                self.table.instructions[idx].addresses[1] = self.label_number_to_address[label_number]

    def __calc_flag(self, args):
        flag = 1 if args[0].isdigit() else 0
        flag += 2 if len(args) > 1 and args[1].isdigit() else 0
        return flag

    def __calc_args(self, flag, args_str):
        args = []
        if flag % 2 == 0:
            args.append(self.__get_var_address(args_str[0]))
        else:
            args.append(int(args_str[0]))
        if len(args_str) > 1:
            if flag // 2 == 0:
                args.append(self.__get_var_address(args_str[1]))
            else:
                args.append(int(args_str[1]))
        return args

    def __find_function_arguments_and_locals(self):
        previous_function_name = None
        locals = []
        for command_words in self.words:
            if command_words[0] == 'FUN':
                previous_function_name = command_words[1]
                argument_names = command_words[2:]
                self.function_to_argument_names[previous_function_name] = argument_names
            elif command_words[0] == 'VAR':
                locals.append(command_words[1])
            else:
                if previous_function_name is not None:
                    self.function_to_local_names[previous_function_name] = locals
                previous_function_name = None
                locals = []

    def parse_glob(self, command_words):
        instruction_address = self.table.size()
        self.globals[command_words[1]] = instruction_address
        return [GlobInstruction()]

    def parse_var(self, command_words):
        var_address = self.table.push_to_stack()
        self.locals[command_words[1]] = var_address
        return []

    def parse_print(self, command_words):
        instructions = []
        arg = command_words[1]
        if arg[0] == '\"' and arg[-1] == '\"':
            for symbol in arg[1:-1]:
                instructions.append(PrintInstruction(flag=1, value=ord(symbol)))
        else:
            address = self.__get_var_address(arg)
            print(address)
            instructions.append(PrintInstruction(flag=0, addresses=[0, address]))
        return instructions

    def parse_move(self, command_words):
        address_to = self.__get_var_address(command_words[1])
        flag = self.__calc_flag(command_words[2])
        address_from = self.__calc_args(flag, [command_words[2]])[0]
        return [MoveInstruction(flag=flag, addresses=[address_to, address_from])]

    def parse_label(self, command_words):
        label_name = command_words[1]
        label_number = self.__get_label_number_or_add(label_name)
        self.label_number_to_address[label_number] = self.table.size()
        return []

    def parse_jump(self, command_words):
        label_number = self.__get_label_number_or_add(command_words[1])
        self.instructions_with_wrong_addresses.append((len(self.table.instructions), False, True))
        return [JumpInstruction(addresses=[0, label_number])]

    def parse_if(self, command_words):
        instructions = []
        operator = logical_operator_name_to_logical_operator[command_words[1]]

        compared_value_str = command_words[2]
        address_to = self.table.get_arithmetic_result_address()
        flag = self.__calc_flag(compared_value_str)
        address_from = self.__calc_args(flag, [compared_value_str])[0]
        instructions.append(MoveInstruction(flag=flag, addresses=[address_to, address_from]))

        address_true = self.__get_label_number_or_add(command_words[3])
        cjmp_instruction_index = len(self.table.instructions) + len(instructions)
        has_left_address = len(command_words) >= 5
        if has_left_address:
            operator = CjumpInstruction.add_else_to_flag(operator)
            address_false = self.__get_label_number_or_add(command_words[4])
        else:
            address_false = 0
            self.instructions_with_wrong_addresses.append((len(self.table.instructions), False, True))
        self.instructions_with_wrong_addresses.append((cjmp_instruction_index, len(command_words) >= 5, True))
        instructions.append(CjumpInstruction(flag=operator, addresses=[address_false, address_true]))
        return instructions

    def parse_read(self, command_words):
        address_to = self.__get_var_address(command_words[2])
        return [ReadInstruction(addresses=[address_to])]

    def parse_fun(self, command_words):
        label_name = command_words[1]
        label_number = self.__get_label_number_or_add(label_name)
        self.label_number_to_address[label_number] = self.table.size_without_stack()
        return []

    def parse_call(self, command_words):
        instructions = []
        local_name_to_address = {}
        arg_name_to_address = {}
        function_name = command_words[1]
        for arg_name in command_words[2:-2:-1]:
            var_address = self.__get_var_address(arg_name)
            arg_address = self.table.push_to_stack(0)
            self.table.instructions.append(MoveInstruction(flag=0, addresses=[arg_address, var_address]))
            arg_name_to_address[arg_name] = arg_address
        self.table.push_to_stack(self.table.size_without_stack())

        for local_name in self.function_to_local_names[function_name]:
            arg_address = self.table.push_to_stack(0)
            local_name_to_address[local_name] = arg_address

        label_number = self.__get_label_number_or_add(command_words[1])
        self.instructions_with_wrong_addresses.append((len(self.table.instructions), False, True))
        instructions.append(JumpInstruction(addresses=[0, label_number]))

        self.local_name_to_address_stack.append(local_name_to_address)
        self.arg_name_to_address_stack.append(arg_name_to_address)

    def parse_ret(self, command_words):
        for idx in range(len(self.local_name_to_address_stack)):
            self.table.pop_from_stack()
        flag = self.__calc_flag(command_words[1:2])
        address_from = self.__calc_args(flag, command_words[1:2])[0]
        address_to = self.table.get_arithmetic_result_address()
        self.table.instructions.append(MoveInstruction(flag, addresses=[address_to, address_from]))
        jmp_instruction = JumpInstruction(value=self.table.stack[-1].value)
        self.table.instructions.append(jmp_instruction)
        self.local_name_to_address_stack.pop()

    def parse_arithmetic(self, command_words):
        instructions = []

        arithmetic_instruction = command_name_to_arithmetic_instruction.get(command_words[0])
        arg_words = command_words[1:3]
        flag = self.__calc_flag(arg_words)
        args = self.__calc_args(flag, arg_words)
        instructions.append(arithmetic_instruction(flag, addresses=args))

        result_name = command_words[4]
        result_address = self.__get_var_address(result_name)
        addresses = [result_address, self.table.get_arithmetic_result_address()]
        instructions.append(MoveInstruction(flag=0, addresses=addresses))
        return instructions

    def parse_table(self):
        table = self.table
        for command_words in self.words:
            print(command_words)
            instructions = self.command_name_to_parser[command_words[0]](command_words)
            table.instructions += instructions
        self.__replace_label_numbers()

    def write(self, path_to):
        with open(path_to, 'wb') as fout:
            for instruction in self.table.instructions:
                fout.write(Serializer.to_byte_string(instruction))


if __name__ == '__main__':
    # assembler = Assembler('data/fibonacci.txt')
    assembler = Assembler('data/simple.txt')
    assembler.parse_table()
    # assembler.write('data/fib')
