import shlex
import numpy as np

from table import Table

from instructions.memory import GlobInstruction, PushInstruction, PopInstruction
from instructions.arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from instructions.io import PrintInstruction, ReadInstruction
from instructions.other import MoveInstruction, JumpInstruction, CjumpInstruction
from instructions.other import logical_operator_name_to_logical_operator

from instructions.enums import InstructionFlag, PrintEnum

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
    def to_byte_array(instruction):
        result = np.array([instruction.get_type(), instruction.flag], dtype=np.ubyte)
        if instruction.addresses is not None:
            addresses = np.array(list(map(lambda address: int_to_byte_array(address, 2),
                                          instruction.addresses)), dtype=np.ubyte).flatten()
            result = np.concatenate((result, addresses))
        else:
            value = int_to_byte_array(instruction.value, 4)
            result = np.append(result, value)
        return result

    @staticmethod
    def to_byte_string(instruction):
        return Serializer.to_byte_array(instruction).tobytes()


class Assembler(object):
    def __init__(self, path_from):
        with open(path_from, 'r') as fin:
            lines = filter(bool, map(str.strip, fin.readlines()))
        self.words = list(map(shlex.split, lines))
        print(self.words)
        self.table = Table()

        self.label_name_to_number = {}
        self.label_number_to_address = {}
        self.instructions_with_wrong_addresses = []

        self.global_to_address = {}
        # self.local_to_index = []
        self.arg_to_address_stack = []

        # self.args_passed_addresses = []
        self.function_to_arguments_offsets = {}
        self.function_to_locals_offsets = {}
        self.current_function_name = None

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

    #
    def __find_function_arguments(self):
        for command_words in self.words:
            if command_words[0] == 'FUN':
                function_name = command_words[1]
                argument_names = command_words[2:]
                offsets = {arg_name: len(argument_names)-idx-1 for idx, arg_name in enumerate(argument_names)}
                self.function_to_arguments_offsets[function_name] = offsets

    #
    def __find_function_locals(self):
        function_name = None
        for command_words in self.words:
            if command_words[0] == 'FUN':
                function_name = command_words[1]
            if command_words[0] == 'VAR':
                offsets = self.function_to_locals_offsets.setdefault(function_name, {})
                self.function_to_locals_offsets[function_name][command_words[1]] = len(offsets)
        for function_name, offsets in self.function_to_locals_offsets.items():
            for local_name in offsets.keys():
                offsets[local_name] = len(offsets) - offsets[local_name] - 1

    def __put_var_address_to_arithmetic_glob(self, name):
        result = None
        local_offsets = self.function_to_locals_offsets[self.current_function_name]

        local_offset = local_offsets.get(name, None)
        argument_offset = self.function_to_arguments_offsets[self.current_function_name].get(name, None)
        global_address = self.global_to_address.get(name, None)
        if argument_offset is not None:
            argument_offset += len(local_offsets) + 1  # +1 because of return address

        stack_offset = local_offset or argument_offset
        if stack_offset is not None:
            sp = self.table.get_stack_pointer_address()
            self.table.instructions.append(SubInstruction(flag=InstructionFlag.FIRST_ARG_IS_ADDR,
                                                          addresses=[sp, stack_offset]))
        elif global_address is not None:
            address_target = self.table.get_arithmetic_glob_address()
            self.table.instructions.append(MoveInstruction(flag=InstructionFlag.FIRST_ARG_IS_ADDR,
                                                           addresses=[address_target, global_address]))
        return self.table.get_arithmetic_glob_address()

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
        flag = InstructionFlag.ARGS_ARE_VALUES if args[0].isdigit() else InstructionFlag.FIRST_ARG_IS_ADDR
        if len(args) > 1 and not args[-1].isdigit():
            flag |= InstructionFlag.LAST_ARG_IS_ADDR
        return flag

    # def __calc_args(self, flag, args_str):
    #     args = []
    #     if flag == InstructionFlag.FIRST_ARG_IS_ADDR:
    #         args.append(self.__get_var_address(args_str[0]))
    #     else:
    #         args.append(int(args_str[0]))
    #     if len(args_str) > 1:
    #         if flag // 2 == 0:
    #             args.append(self.__get_var_address(args_str[1]))
    #         else:
    #             args.append(int(args_str[1]))
    #     return args

    def __jump_to_label(self, label_name):
        label_number = self.__get_label_number_or_add(label_name)
        self.instructions_with_wrong_addresses.append((len(self.table.instructions), False, True))
        self.table.instructions.append(JumpInstruction(addresses=[0, label_number]))

    # 1
    def parse_glob(self, command_words):
        instruction_address = self.table.size(need_stack=False)
        self.global_to_address[command_words[1]] = instruction_address
        self.table.instructions.append(GlobInstruction())

    # 1
    def parse_var(self, command_words):
        pass

    # 1
    def parse_print(self, command_words):
        arg = command_words[1]
        if arg[0] == '\"' and arg[-1] == '\"':
            for symbol in arg[1:-1]:
                self.table.instructions.append(PrintInstruction(flag=PrintEnum.CHAR, value=ord(symbol)))
        elif arg[0].isletter():
            address = self.__put_var_address_to_arithmetic_glob(arg)
            self.table.instructions.append(PrintInstruction(flag=PrintEnum.ADDR, addresses=[0, address]))
        else:
            self.table.instructions.append(PrintInstruction(flag=PrintEnum.NUMBER, value=int(arg)))

    #  1/2
    def parse_move(self, command_words):
        address_to = self.__put_var_address_to_arithmetic_glob(command_words[1])
        flag = self.__calc_flag(command_words[1:3])
        address_from = self.__calc_args(flag, [command_words[2]])[0]
        self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address_to, address_from]))

    def parse_fun(self, command_words):
        self.current_function_name = command_words[1]
        self.parse_label(command_words)

    #  1/2
    def parse_label(self, command_words):
        label_name = command_words[1]
        label_number = self.__get_label_number_or_add(label_name)
        self.label_number_to_address[label_number] = self.table.size()

    #  1/2
    def parse_jump(self, command_words):
        self.__jump_to_label(command_words[1])

    #  1/2
    def parse_if(self, command_words):
        operator = logical_operator_name_to_logical_operator[command_words[1]]

        compared_value_str = command_words[2]
        address_to = self.table.get_arithmetic_glob_address()
        flag = self.__calc_flag(compared_value_str)
        address_from = self.__calc_args(flag, [compared_value_str])[0]
        self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address_to, address_from]))

        address_then = self.__get_label_number_or_add(command_words[3])
        has_else_branch = len(command_words) >= 5
        if has_else_branch:
            #  has else branch
            operator = CjumpInstruction.add_else_to_flag(operator)
            address_else = self.__get_label_number_or_add(command_words[4])
        else:
            address_else = 0
        self.instructions_with_wrong_addresses.append((len(self.table.instructions), has_else_branch, True))
        self.table.instructions.append(CjumpInstruction(flag=operator, addresses=[address_else, address_then]))

    #  1/2
    def parse_read(self, command_words):
        address_to = self.__get_var_address(command_words[2])
        self.table.instructions.append(ReadInstruction(addresses=[0, address_to]))

    #
    def parse_call(self, command_words):
        function_name, args_passed, result_var = command_words[1], command_words[2:-2], command_words[-1]

        #  pass arguments
        args_received = self.function_to_arguments[function_name]
        self.arg_to_address_stack.append({})
        for arg_received, arg_passed in zip(args_received[::-1], args_passed[::-1]):
            arg_passed_address = self.__get_var_address(arg_passed)
            arg_address = self.table.push_to_stack(0)
            self.table.instructions.append(MoveInstruction(flag=0, addresses=[arg_address, arg_passed_address]))
            self.arg_to_address_stack[-1][arg_received] = arg_address

        # prepare dict for locals
        self.local_to_address_stack.append({})

        #  jump to label
        self.__jump_to_label(function_name)

        #  push return value
        self.table.push_to_stack(self.table.size(need_stack=False))

        #  save result
        result_address = self.__get_var_address(result_var)
        self.table.instructions.append(MoveInstruction(flag=0, addresses=[result_address,
                                                                          self.table.get_arithmetic_glob_address()]))

        #  remove return value from stack
        self.table.pop_from_stack()
        #  remove args from stack
        for _ in range(self.arg_to_address_stack[-1]):
            self.table.pop_from_stack()
        # remove dicts
        self.arg_to_address_stack.pop()
        self.local_to_address_stack.pop()

    #
    def parse_ret(self, command_words):
        flag = self.__calc_flag(command_words[1:2])
        address_from = self.__calc_args(flag, command_words[1:2])[0]
        address_to = self.table.get_arithmetic_glob_address()
        self.table.instructions.append(MoveInstruction(flag, addresses=[address_to, address_from]))

        for idx in range(len(self.local_to_address_stack[-1])):
            self.table.pop_from_stack()
        jmp_instruction = JumpInstruction(value=self.table.stack[-1].value)
        self.table.instructions.append(jmp_instruction)
        self.local_to_address_stack.pop()

    #  1/2
    def parse_arithmetic(self, command_words):
        arithmetic_instruction = command_name_to_arithmetic_instruction.get(command_words[0])
        arg_words = command_words[1:3]
        flag = self.__calc_flag(arg_words)
        addresses = self.__calc_args(flag, arg_words)
        self.table.instructions.append(arithmetic_instruction(flag, addresses=addresses))

        result_name = command_words[4]
        result_address = self.__get_var_address(result_name)
        addresses = [result_address, self.table.get_arithmetic_glob_address()]
        self.table.instructions.append(MoveInstruction(flag=0, addresses=addresses))

    def parse_table(self):
        self.__find_function_arguments()
        self.__find_function_locals()
        print('Locals:')
        print(self.function_to_locals_offsets)
        print('Arguments:')
        print(self.function_to_arguments_offsets)
        return
        for command_words in self.words:
            print(command_words)
            self.command_name_to_parser[command_words[0]](command_words)
        self.__replace_label_numbers()

    def write(self, path_to):
        with open(path_to, 'wb') as fout:
            for idx, instruction in enumerate(self.table.instructions):
                print(idx * 6, ': ', Serializer.to_byte_array(instruction))
                fout.write(Serializer.to_byte_string(instruction))


if __name__ == '__main__':
    assembler = Assembler('data/fibonacci.txt')
    # assembler = Assembler('data/simple.txt')
    assembler.parse_table()
    assembler.write('data/simple')
