import numpy as np
from collections import namedtuple

from circuit import NotNode, InputNode, AndNode
from permutation import Permutation


class BranchingProgram:
    def __init__(self):
        pass

    @staticmethod
    def build_from_permuting_branching_program(pbp_program):
        for var, perm_false, perm_true in pbp_program.instructions:
            pass


class PermutingBranchingProgram:
    Instruction = namedtuple('Instruction', ['var', 'perm_false', 'perm_true'])

    @staticmethod
    def build_from_circuit(circuit, sigma=None):
        node_to_program = {}
        sigma = sigma or Permutation(np.array([1, 2, 3, 4, 0]))
        for node_idx, node in circuit.get_nodes().items():
            if isinstance(node, InputNode):
                node_to_program[node_idx] = PermutingBranchingProgram([
                    PermutingBranchingProgram.Instruction(node.get_name(),
                                                          Permutation.identity(5),
                                                          sigma)
                ], sigma)
            elif isinstance(node, NotNode):
                node_input = circuit.get_inputs()[node_idx][0]
                node_to_program[node_idx] = node_to_program[node_input].invert()
            elif isinstance(node, AndNode):
                node_inputs = circuit.get_inputs()[node_idx]
                node_to_program[node_idx] = node_to_program[node_inputs[0]].intersect(node_to_program[node_inputs[1]])
            else:
                raise ValueError('Unsupported node type')
        return node_to_program[circuit.get_terminal_node()]

    def __init__(self, instructions, final_permutation):
        self.instructions = instructions
        self._sigma = final_permutation

    def get_sigma(self):
        return self._sigma

    def change_sigma(self, sigma_new):
        instructions = [None] * len(self.instructions)
        if len(instructions) == 1:
            if self.instructions[0].perm_false == Permutation.identity(len(self.instructions[0].perm_false.get())):
                instructions[0] = self.Instruction(self.instructions[0].var,
                                                   self.instructions[0].perm_false,
                                                   sigma_new)
            else:
                instructions[0] = self.Instruction(self.instructions[0].var,
                                                   sigma_new,
                                                   self.instructions[0].perm_true)
        else:
            gamma = self._sigma.calc_conjugate(sigma_new)
            gamma_inverted = gamma.invert()
            instructions[0] = self.Instruction(self.instructions[0].var,
                                               gamma * self.instructions[0].perm_false,
                                               gamma * self.instructions[0].perm_true)
            instructions[1:-1] = self.instructions[1:-1]
            instructions[-1] = self.Instruction(self.instructions[-1].var,
                                                self.instructions[-1].perm_false * gamma_inverted,
                                                self.instructions[-1].perm_true * gamma_inverted)
        return PermutingBranchingProgram(instructions, sigma_new)

    def invert(self):
        instructions = self.change_sigma(self._sigma.invert()).instructions
        instructions[-1] = self.Instruction(instructions[-1].var,
                                            instructions[-1].perm_false * self._sigma,
                                            instructions[-1].perm_true * self._sigma)
        return PermutingBranchingProgram(instructions, self._sigma)

    def intersect(self, other, preserve_sigma=True, non_commuting_sigma=None):
        sigma_inverted = self._sigma.invert()
        other_sigma_inverted = other.get_sigma().invert()
        if (self._sigma * other.get_sigma() * sigma_inverted * other_sigma_inverted).is_identity():
            non_commuting_sigma = non_commuting_sigma or Permutation(np.array([2, 4, 1, 0, 3]))
            other_sigma_inverted = non_commuting_sigma.invert()
            if (self._sigma * non_commuting_sigma * sigma_inverted * other_sigma_inverted).is_identity():
                raise ValueError('Commuting sigma provided')
            other = other.change_sigma(non_commuting_sigma)
        instructions = [None] * (2 * (len(self.instructions) + len(other.instructions)))
        left, right = 0, len(self.instructions)
        instructions[left:right] = self.instructions
        left, right = right, right + len(other.instructions)
        instructions[left:right] = other.instructions
        left, right = right, right + len(self.instructions)
        instructions[left:right] = self.change_sigma(sigma_inverted).instructions
        left, right = right, right + len(other.instructions)
        instructions[left:right] = other.change_sigma(other_sigma_inverted).instructions
        result = PermutingBranchingProgram(instructions,
                                           self._sigma * other.get_sigma() * sigma_inverted * other_sigma_inverted)
        if preserve_sigma:
            result = result.change_sigma(self._sigma)
        return result
