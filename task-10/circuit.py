from copy import deepcopy


class Node:
    def evaluate(self, *args, **kwargs):
        raise NotImplementedError()


class InputNode(Node):
    def __init__(self, name):
        self._name = name

    def evaluate(self, *args, **kwargs):
        pass

    def get_name(self):
        return self._name


class NotNode(Node):
    def evaluate(self, x, *args, **kwargs):
        return not x


class AndNode(Node):
    def evaluate(self, x, y, *args, **kwargs):
        return x and y


class OrNode(Node):
    def evaluate(self, x, y, *args, **kwargs):
        return x or y


class LogicalCircuit:
    def __init__(self, nodes, inputs, terminal_node=None):
        self._nodes = nodes
        self._max_node_idx = max(nodes.keys())
        self.terminal_node = terminal_node or self._max_node_idx
        self._inputs = inputs

    def get_nodes(self):
        return self._nodes

    def get_inputs(self):
        return self._inputs

    def get_terminal_node(self):
        return self.terminal_node

    def copy(self):
        return LogicalCircuit(*map(deepcopy, [self._nodes, self._inputs, self.terminal_node]))

    def add_node(self, node, inputs=None):
        node_idx = len(self._nodes)
        self._max_node_idx += 1
        self._nodes[self._max_node_idx] = node
        self._inputs[node_idx] = inputs
        return node_idx

    def remove_node(self, node_idx):
        self._nodes.pop(node_idx)
        self._inputs.pop(node_idx)
        if node_idx == self.terminal_node:
            self.terminal_node = None

    def eliminate_or_nodes(self):
        circuit_new = self.copy()
        for node_idx, node in self._nodes.items():
            if isinstance(node, OrNode):
                inputs = self._inputs[node_idx]
                node_not_x_idx = circuit_new.add_node(NotNode(), [inputs[0]])
                node_not_y_idx = circuit_new.add_node(NotNode(), [inputs[1]])
                node_and_idx = circuit_new.add_node(AndNode(), [node_not_x_idx, node_not_y_idx])
                node_final = circuit_new.add_node(NotNode(), [node_and_idx])
                circuit_new.remove_node(node_idx)
                if self.terminal_node is None:
                    circuit_new.terminal_node = node_final
        return circuit_new
