import shlex
import numpy as np

from os.path import join, splitext

from table import Table, INSTRUCTION_LENGTH

from instructions.memory import GlobInstruction, PushInstruction, PopInstruction
from instructions.arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from instructions.io import PrintInstruction, ReadInstruction
from instructions.other import MoveInstruction, JumpInstruction, CjumpInstruction, StopInstruction
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

MAIN_FUNCTION_NAME = '.main'


class Serializer(object):
    @staticmethod
    def to_byte_array(instruction):
        result = np.array([instruction.get_type(), instruction.flag], dtype=np.ubyte)
        if instruction.addresses is not None:
            addresses = np.array(list(map(lambda address: int_to_byte_array(address, 2),
                                          instruction.addresses)), dtype=np.ubyte).flatten()
            result = np.concatenate((result, addresses))
        else:
            if instruction.value is None:
                instruction.value = 0
            value = int_to_byte_array(instruction.value, 4)
            result = np.append(result, value)
        return result

    @staticmethod
    def to_byte_string(instruction):
        return Serializer.to_byte_array(instruction).tobytes()


class Assembler(object):
    def __init__(self, path_from):
        with open(path_from, 'r') as fin:
            lines = filter(bool, map(str.strip, fin.readlines()))  # strip and filter empty lines
            lines = filter(lambda line: line[0] != '#', lines)  # filter comments
        self.words = list(map(shlex.split, lines))
        print(self.words)
        self.table = Table()

        self.main_function_address = None

        self.label_name_to_number = {}
        self.label_number_to_address = {}
        self.instructions_with_wrong_addresses = []

        self.global_to_address = {}

        self.function_to_arguments_names = {}

        self.function_to_arguments_offsets = {}
        self.function_to_locals_offsets = {}
        self.current_function_name = None

        self.command_name_to_parser = {
            'GLOB': self.parse_glob,
            'VAR': self.parse_var,

            'PRINTLN': self.parse_print,
            'PRINT': self.parse_print,
            'READ': self.parse_read,

            'MOVE': self.parse_move,
            'LABEL': self.parse_label,
            'JUMP': self.parse_jump,
            'IF': self.parse_if,

            'FUN': self.parse_fun,
            'CALL': self.parse_call,
            'RET': self.parse_ret,
            'EXIT': self.parse_exit,

            'ADD': self.parse_arithmetic,
            'SUB': self.parse_arithmetic,
            'MUL': self.parse_arithmetic,
            'DIV': self.parse_arithmetic,
            'MOD': self.parse_arithmetic
        }

    def __search_functions(self):
        function_name = None
        for command_words in self.words:
            if command_words[0] == 'FUN':
                function_name = command_words[1]
                argument_names = command_words[2:]
                offsets = {arg_name: len(argument_names)-idx-1 for idx, arg_name in enumerate(argument_names)}
                self.function_to_arguments_names[function_name] = argument_names
                self.function_to_arguments_offsets[function_name] = offsets
                self.function_to_locals_offsets[function_name] = {}
            if command_words[0] == 'VAR':
                offsets = self.function_to_locals_offsets.setdefault(function_name, {})
                self.function_to_locals_offsets[function_name][command_words[1]] = len(offsets)
        for function_name, offsets in self.function_to_locals_offsets.items():
            for local_name in offsets.keys():
                offsets[local_name] = len(offsets) - offsets[local_name] - 1

    def __put_var_address_to_arithmetic_glob(self, name):
        local_offsets = self.function_to_locals_offsets.get(self.current_function_name, None)
        argument_offsets = self.function_to_arguments_offsets.get(self.current_function_name, None)

        if local_offsets is not None:
            local_offset = local_offsets.get(name, None)
            argument_offset = argument_offsets.get(name, None)
        else:
            local_offset = None
            argument_offset = None
        if argument_offset is not None:
            if local_offsets is not None:
                argument_offset += len(local_offsets)
            argument_offset += 1  # +1 because of return address

        global_address = self.global_to_address.get(name, None)
        stack_offset = local_offset if local_offset is not None else argument_offset
        if stack_offset is not None:
            sp = self.table.get_stack_pointer_address()
            self.table.instructions.append(SubInstruction(flag=InstructionFlag.FIRST_ARG_IS_ADDR,
                                                          addresses=[sp, stack_offset * INSTRUCTION_LENGTH]))
        elif global_address is not None:
            address_target = self.table.get_arithmetic_glob_address()
            flag = InstructionFlag.FIRST_ARG_IS_ADDR
            self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address_target, global_address]))
        return self.table.get_arithmetic_glob_address()

    def __save_arithmetic_glob_to_temporary_glob(self):
        address_to = self.table.get_temporary_glob_address()
        address_from = self.table.get_arithmetic_glob_address()
        flag = InstructionFlag.FIRST_ARG_IS_ADDR | InstructionFlag.LAST_ARG_IS_ADDR
        self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address_to, address_from]))
        return self.table.get_temporary_glob_address()

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

    def __calc_flag(self, arg, is_first_arg=False):
        flag = InstructionFlag.ARGS_ARE_VALUES
        if not arg.isdigit():
            flag = InstructionFlag.FIRST_ARG_IS_ADDR if is_first_arg else InstructionFlag.LAST_ARG_IS_ADDR
        return flag

    def __jump_to_label(self, label_name):
        label_number = self.__get_label_number_or_add(label_name)
        self.instructions_with_wrong_addresses.append((len(self.table.instructions), False, True))
        self.table.instructions.append(JumpInstruction(addresses=[0, label_number]))

    def parse_glob(self, command_words):
        instruction_address = self.table.size(need_stack=False)
        self.global_to_address[command_words[1]] = instruction_address
        self.table.instructions.append(GlobInstruction())

    def parse_var(self, command_words):
        pass

    def parse_print(self, command_words):
        arg = command_words[1]
        if arg[0] == '\"' and arg[-1] == '\"' or arg[0] == '\'' and arg[-1] == '\'':
            for symbol in arg[1:-1]:
                self.table.instructions.append(PrintInstruction(flag=PrintEnum.CHAR, value=ord(symbol)))
            if command_words[0].lower()[-2:] == 'ln':
                self.table.instructions.append(PrintInstruction(flag=PrintEnum.CHAR, value=ord('\n')))
        elif arg[0].isalpha():
            address = self.__put_var_address_to_arithmetic_glob(arg)
            self.table.instructions.append(PrintInstruction(flag=PrintEnum.ADDR_OF_ADDR, addresses=[0, address]))
        else:
            self.table.instructions.append(PrintInstruction(flag=PrintEnum.NUMBER, value=int(arg)))

    def parse_move(self, command_words):
        self.__put_var_address_to_arithmetic_glob(command_words[1])
        address_to = self.__save_arithmetic_glob_to_temporary_glob()

        address_from = self.__put_var_address_to_arithmetic_glob(command_words[2])
        flag = InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR | InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR

        self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address_to, address_from]))

    def parse_fun(self, command_words):
        self.current_function_name = command_words[1]
        if command_words[1] == MAIN_FUNCTION_NAME:
            self.main_function_address = self.table.size(need_stack=False)
        self.parse_label(command_words)

    def parse_label(self, command_words):
        label_name = command_words[1]
        label_number = self.__get_label_number_or_add(label_name)
        self.label_number_to_address[label_number] = self.table.size()

    def parse_jump(self, command_words):
        self.__jump_to_label(command_words[1])

    def parse_if(self, command_words):
        operator = logical_operator_name_to_logical_operator[command_words[1]]

        compared_value_str = command_words[2]
        if compared_value_str.isdigit():
            flag = InstructionFlag.FIRST_ARG_IS_ADDR
            address = self.table.get_arithmetic_glob_address()
            value = int(compared_value_str)
            self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address, value]))
        else:
            self.__put_var_address_to_arithmetic_glob(compared_value_str)
            flag = InstructionFlag.FIRST_ARG_IS_ADDR | InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR
            address = self.table.get_arithmetic_glob_address()
            self.table.instructions.append(MoveInstruction(flag=flag, addresses=[address, address]))

        address_then = self.__get_label_number_or_add(command_words[3])
        has_else_branch = len(command_words) >= 5
        if has_else_branch:
            operator = CjumpInstruction.add_else_to_flag(operator)
            address_else = self.__get_label_number_or_add(command_words[4])
        else:
            address_else = 0
        self.instructions_with_wrong_addresses.append((len(self.table.instructions), has_else_branch, True))
        self.table.instructions.append(CjumpInstruction(flag=operator, addresses=[address_else, address_then]))

    def parse_read(self, command_words):
        address_to = self.__put_var_address_to_arithmetic_glob(command_words[2])
        flag = InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR
        self.table.instructions.append(ReadInstruction(flag=flag, addresses=[0, address_to]))

    def parse_call(self, command_words):
        function_name, args_passed, result_var = command_words[1], command_words[2:-2], command_words[-1]

        # pass arguments
        arg_names = self.function_to_arguments_names[function_name]
        for arg_name, arg_passed in zip(arg_names, args_passed):
            arg_passed_address = self.__put_var_address_to_arithmetic_glob(arg_passed)
            flag = InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR
            self.table.instructions.append(PushInstruction(flag=flag, addresses=[0, arg_passed_address]))

        local_offsets = self.function_to_locals_offsets[function_name]

        # push return address
        flag = InstructionFlag.ARGS_ARE_VALUES
        # current size + push return address + push all locals + jump
        return_address = self.table.size(need_stack=False) + (len(local_offsets) + 2) * INSTRUCTION_LENGTH
        self.table.instructions.append(PushInstruction(flag=flag, addresses=[0, return_address]))

        # push locals
        for _ in local_offsets:
            self.table.instructions.append(PushInstruction(flag=InstructionFlag.ARGS_ARE_VALUES, value=0))

        # jump to label
        self.__jump_to_label(function_name)

        # save result
        return_value_address = self.__save_arithmetic_glob_to_temporary_glob()
        result_address = self.__put_var_address_to_arithmetic_glob(result_var)
        flag = InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR | InstructionFlag.LAST_ARG_IS_ADDR
        self.table.instructions.append(MoveInstruction(flag=flag, addresses=[result_address,
                                                                             return_value_address]))

        # remove return value from stack
        self.table.instructions.append(PopInstruction())
        # remove args from stack
        for _ in arg_names:
            self.table.instructions.append(PopInstruction())

    def parse_ret(self, command_words):
        # save result to arithmetic glob
        if command_words[1].isdigit():
            address_to = self.table.get_arithmetic_glob_address()
            value = int(command_words[1])
            self.table.instructions.append(MoveInstruction(flag=InstructionFlag.FIRST_ARG_IS_ADDR,
                                                           addresses=[address_to, value]))
        else:
            return_value_address = self.__put_var_address_to_arithmetic_glob(command_words[1])
            flag = InstructionFlag.FIRST_ARG_IS_ADDR | InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR
            self.table.instructions.append(MoveInstruction(flag=flag,
                                                           addresses=[return_value_address, return_value_address]))

        # remove locals from stack
        for _ in self.function_to_locals_offsets[self.current_function_name]:
            self.table.instructions.append(PopInstruction())

        # jump back
        flag = InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR
        address = self.table.get_stack_pointer_address()
        self.table.instructions.append(JumpInstruction(flag=flag, addresses=[0, address]))

    def parse_arithmetic(self, command_words):
        arithmetic_instruction = command_name_to_arithmetic_instruction.get(command_words[0])
        flag = self.__calc_flag(command_words[1], is_first_arg=True)
        flag |= self.__calc_flag(command_words[2], is_first_arg=False)

        flag_arithm = InstructionFlag.ARGS_ARE_VALUES
        if flag | InstructionFlag.FIRST_ARG_IS_ADDR:
            self.__put_var_address_to_arithmetic_glob(command_words[1])
            address_x = self.__save_arithmetic_glob_to_temporary_glob()
            flag_arithm |= InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR
        else:
            address_x = int(command_words[1])

        if flag | InstructionFlag.LAST_ARG_IS_ADDR:
            address_y = self.__put_var_address_to_arithmetic_glob(command_words[2])
            flag_arithm |= InstructionFlag.LAST_ARG_IS_ADDR_OF_ADDR
        else:
            address_y = int(command_words[2])

        self.table.instructions.append(arithmetic_instruction(flag=flag_arithm, addresses=[address_x, address_y]))

        address_from = self.__save_arithmetic_glob_to_temporary_glob()
        address_to = self.__put_var_address_to_arithmetic_glob(command_words[4])
        flag_move = InstructionFlag.FIRST_ARG_IS_ADDR_OF_ADDR | InstructionFlag.LAST_ARG_IS_ADDR
        self.table.instructions.append(MoveInstruction(flag=flag_move, addresses=[address_to, address_from]))

    def parse_exit(self):
        self.table.instructions.append(StopInstruction())

    def parse_table(self):
        self.__search_functions()
        print('Locals:')
        print(self.function_to_locals_offsets)
        print('Arguments:')
        print(self.function_to_arguments_offsets)
        for command_words in self.words:
            print(command_words)
            self.command_name_to_parser[command_words[0]](command_words)
        self.table.instructions.append(StopInstruction())
        self.__replace_label_numbers()
        assert self.main_function_address, 'No main function has been found'
        idx = self.table.get_instruction_index()
        self.table.instructions[idx].value = self.main_function_address

    def write(self, path_to):
        with open(path_to, 'wb') as fout:
            for idx, instruction in enumerate(self.table.instructions):
                print(idx * 6, ': ', Serializer.to_byte_array(instruction))
                fout.write(Serializer.to_byte_string(instruction))


if __name__ == '__main__':
    input_dir, output_dir = 'data', 'translated'
    input_file_name = 'equal.txt'
    input_file = join(input_dir, input_file_name)
    output_file = join(output_dir, splitext(input_file_name)[0])
    assembler = Assembler(input_file)
    assembler.parse_table()
    assembler.write(output_file)
