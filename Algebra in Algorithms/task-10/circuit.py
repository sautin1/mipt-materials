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
    def __init__(self, nodes, inputs, terminal_node=None, eliminate_or_nodes=False):
        self._nodes = nodes
        self._terminal_node = terminal_node or (len(nodes) - 1)
        self._inputs = inputs
        if eliminate_or_nodes:
            self._eliminate_or_nodes()

    def get_nodes(self):
        return self._nodes

    def get_inputs(self):
        return self._inputs

    def get_terminal_node(self):
        return self._terminal_node

    def copy(self):
        return LogicalCircuit(*map(deepcopy, [self._nodes, self._inputs, self._terminal_node]))

    def get_topological_order(self):
        def traverse_dfs(node, visited, nodes_black):
            visited[node] = True
            for node_input in self._inputs[node]:
                if not visited[node_input]:
                    traverse_dfs(node_input, visited, nodes_black)
            nodes_black.append(node)

        topological_order = []
        is_visited = [False] * len(self._nodes)
        for node_idx in range(len(self._nodes)):
            if not is_visited[node_idx]:
                traverse_dfs(node_idx, is_visited, topological_order)
        return topological_order

    def _add_node(self, node, inputs=None):
        inputs = inputs or []
        self._nodes.append(node)
        self._inputs.append(inputs)
        return len(self._nodes) - 1

    def _eliminate_or_nodes(self):
        node_indices = list(range(len(self._nodes)))
        for node_idx in node_indices:
            node = self._nodes[node_idx]
            if isinstance(node, OrNode):
                inputs = self._inputs[node_idx]
                node_not_x_idx = self._add_node(NotNode(), [inputs[0]])
                node_not_y_idx = self._add_node(NotNode(), [inputs[1]])
                node_and_idx = self._add_node(AndNode(), [node_not_x_idx, node_not_y_idx])
                self._nodes[node_idx] = NotNode()
                self._inputs[node_idx] = [node_and_idx]
        return self
