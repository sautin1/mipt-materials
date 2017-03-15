import shlex
import numpy as np

from machine import Table

from instructions.memory import GlobInstruction
from instructions.arithmetics import AddInstruction, SubInstruction, MulInstruction, DivInstruction, ModInstruction
from instructions.io import PrintInstruction, ReadInstruction
from instructions.other import MoveInstruction

from byte_utils import int_to_byte_array

# def is_label(command_words):
#     return command_words[0][0] == '.'

command_source_name_to_arithmetic_instruction = {
    'ADD': AddInstruction,
    'SUB': SubInstruction,
    'MUL': MulInstruction,
    'DIV': DivInstruction,
    'MOD': ModInstruction
}


class Assembler(object):
    def __init__(self, path_from):
        self.words = []
        with open(path_from, 'r') as fin:
            lines = map(str.strip, fin.readlines())
        self.words = list(map(shlex.split, lines))
        self.table = Table()
        self.globals = {}
        self.locals = {}
        self.args = {}

    # def parse_add(self, words):
    #     flag = 1 if words[1].isdigit() else 0
    #     flag += 2 if words[2].isdigit() else 0
    #     arg1 = self.locals[words[1]] if flag % 2 == 0 else int(words[1])
    #     arg2 = self.locals[words[2]] if flag // 2 == 0 else int(words[2])
    #     return
    def get_var_address(self, name):
        return self.locals.get(name) or self.args.get(name) or self.globals.get(name)

    def calc_flag(self, args):
        flag = 1 if args[0].isdigit() else 0
        flag += 2 if len(args) > 1 and args[1].isdigit() else 0
        return flag

    def calc_args(self, flag, args_str):
        args = []
        if flag % 2 == 0:
            args.append(self.get_var_address(args_str[0]))
        else:
            args += list(int_to_byte_array(int(args_str[0]), 2))
        if len(args_str) > 1:
            if flag // 2 == 0:
                args.append(self.get_var_address(args_str[1]))
            else:
                args += list(int_to_byte_array(int(args_str[1]), 2))
        return np.array(args)

    def parse_table(self):
        table = self.table
        table.instructions.append(GlobInstruction())
        for command_words in self.words:
            print(command_words)
            arithmetic_instruction = command_source_name_to_arithmetic_instruction.get(command_words[0])
            if command_words[0] == 'GLOB':
                instruction_address = table.size()
                self.globals[command_words[0]] = instruction_address
                table.instructions.append(GlobInstruction())
            elif arithmetic_instruction is not None:
                arg_words = command_words[1:3]
                flag = self.calc_flag(arg_words)
                args = self.calc_args(flag, arg_words)
                table.instructions.append(arithmetic_instruction(flag, args))
                result_name = command_words[4]
                result_address = self.get_var_address(result_name)
                args = int_to_byte_array(result_address, 2) + int_to_byte_array(table.get_arithmetic_result_address(), 2)
                table.instructions.append(MoveInstruction(0, args))
            elif command_words[0] == 'PRINT':
                arg = command_words[1]
                if arg[0] == '\"' and arg[-1] == '\"':
                    for symbol in arg[1:-1]:
                        table.instructions.append(PrintInstruction(1, int_to_byte_array(ord(symbol), 4)))
                elif:
                    pass
                    # self.calc_args(arg)
                    # table.instructions.append(PrintInstruction(0, ))
                # arg_words = command_words[1:2]
                # flag = self.calc_flag(arg_words)
                # args = self.calc_args(flag, arg_words)
                # table.instructions.append(PrintInstruction(flag, args))

            # self.table.instructions.append()

    def write(self, path_to):
        with open(path_to, 'wb') as fout:
            for instruction in self.table.instructions:
                fout.write(instruction.to_bytes())


if __name__ == '__main__':
    # assembler = Assembler('data/fibonacci.txt')
    assembler = Assembler('data/simple.txt')
    assembler.parse_table()
    # assembler.write('data/fib')
