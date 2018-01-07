import numpy as np
from collections import namedtuple

from circuit import NotNode, InputNode, AndNode
from permutation import Permutation


class BranchingProgram:
    @staticmethod
    def build_from_permuting_branching_program(pbp_program, remove_unreachable=True, reduce_outputs=True):
        sigma = pbp_program.get_sigma()
        node_input = next(node for node in range(len(sigma)) if node != sigma[node])
        node_count = ((len(pbp_program.instructions) + 1) * len(sigma))
        graph = [None] * node_count
        node_labels = [None if node < node_count - len(sigma) else True for node in range(node_count)]
        node_output_false = len(pbp_program.instructions) * len(sigma) + node_input
        node_labels[node_output_false] = False
        for layer_idx, (var, perm_false, perm_true) in enumerate(pbp_program.instructions):
            for node_idx_in_layer in range(len(sigma)):
                node = layer_idx * len(sigma) + node_idx_in_layer
                graph[node] = {
                    False: (layer_idx + 1) * len(sigma) + perm_false[node_idx_in_layer],
                    True: (layer_idx + 1) * len(sigma) + perm_true[node_idx_in_layer]
                }
                node_labels[node] = var
        graph[-len(sigma):] = [{False: None, True: None} for _ in range(len(sigma))]
        return BranchingProgram(graph, node_labels, node_input, remove_unreachable, reduce_outputs)

    def __init__(self, graph, labels, node_input=0, remove_unreachable=True, reduce_outputs=True):
        self._graph = graph
        self._node_labels = labels
        self._node_input = node_input
        if remove_unreachable:
            self._remove_unreachable()
        if reduce_outputs:
            self._reduce_outputs()

    def _remove_unreachable(self):
        def traverse_dfs(node, visited):
            visited[node] = True
            for node_target in self._graph[node].values():
                if node_target is not None and not visited[node_target]:
                    traverse_dfs(node_target, visited)

        is_reachable = [False] * len(self._graph)
        traverse_dfs(self._node_input, is_reachable)
        nodes_reachable = [node for node in range(len(self._graph)) if is_reachable[node]]
        node_renumbering = {node: node_idx_new for node_idx_new, node in enumerate(nodes_reachable)}
        graph = [{
            False: node_renumbering[self._graph[node][False]] if self._graph[node][False] is not None else None,
            True: node_renumbering[self._graph[node][True]] if self._graph[node][True] is not None else None,
        } for node in nodes_reachable]
        node_labels = [self._node_labels[node] for node in nodes_reachable]
        self._graph = graph
        self._node_labels = node_labels
        self._node_input = 0

    def _reduce_outputs(self):
        outputs = {node for node in range(len(self._graph)) if isinstance(self._node_labels[node], bool)}
        output_false = next(output for output in outputs if not self._node_labels[output])
        outputs_true = outputs - {output_false}
        output_false_new = min(outputs)
        output_true_new = output_false_new + 1
        self._graph = self._graph[:output_true_new + 1]
        for node in range(len(self._graph)):
            for edge_type in [False, True]:
                if self._graph[node][edge_type] in outputs_true:
                    self._graph[node][edge_type] = output_true_new
                elif self._graph[node][edge_type] == output_false:
                    self._graph[node][edge_type] = output_false_new
        self._node_labels = self._node_labels[:len(self._graph)]
        self._node_labels[output_false_new], self._node_labels[output_true_new] = False, True

    def get_labels(self):
        return self._node_labels

    def get_graph(self):
        return self._graph

    def get_input(self):
        return self._node_input


class PermutingBranchingProgram:
    Instruction = namedtuple('Instruction', ['var', 'perm_false', 'perm_true'])

    @staticmethod
    def build_from_circuit(circuit, sigma=None):
        node_to_program = {}
        sigma = sigma or Permutation(np.array([1, 2, 3, 4, 0]))
        topological_order = circuit.get_topological_order()
        for node_idx in topological_order:
            node = circuit.get_nodes()[node_idx]
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
