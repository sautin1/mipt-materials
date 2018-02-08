from collections import namedtuple


class Node:
    def execute(self, *args):
        raise NotImplementedError()


class InputNode(Node):
    def __str__(self):
        return 'INP'

    def execute(self, *args):
        pass


class AndNode(Node):
    def __str__(self):
        return 'AND'

    def execute(self, *args):
        x, y = args
        return x & y


class OrNode(Node):
    def __str__(self):
        return 'OR'

    def execute(self, *args):
        x, y = args
        return x | y


class NotNode(Node):
    def __init__(self, max_value):
        self.max_value = max_value

    def __str__(self):
        return 'NOT'

    def execute(self, *args):
        return ~args[0] & self.max_value


class Multiport3Inputs:
    INPUT_COUNT = 3
    FUNCTION_ID_LENGTH = 2 ** INPUT_COUNT
    MAX_FUNCTION_ID = 2 ** FUNCTION_ID_LENGTH - 1
    Edge = namedtuple('Edge', ['source', 'target'])

    def __init__(self):
        self.nodes = {}  # node_id -> Node object
        self.edges = []  # list of node_id pairs: (from, to)
        self.function_id_to_node_id = {}  # bitmask of the output of a boolean function -> node_id
        self.node_id_to_function_id = {}  # reversed self.function_id_to_node_id

        self._build()

    def _build(self):
        input_node_ids = self._add_inputs()
        # add input negations
        self._add_negations(input_node_ids)
        # add and's
        nodes_single_one = self._add_conjunctions(list(self.nodes.keys()))
        # add or's
        self._add_disjunctions(nodes_single_one)

    def _add_inputs(self):
        input_function_ids = [0b11110000, 0b11001100, 0b10101010]
        input_node_ids = []
        for i, function_id in enumerate(input_function_ids):
            input_node_id = self._add_node(InputNode(), function_id)
            input_node_ids.append(input_node_id)
        return input_node_ids

    def _add_negations(self, node_ids):
        not_node_ids = []
        for node_id in node_ids:
            not_node = NotNode(Multiport3Inputs.MAX_FUNCTION_ID)
            not_node_function_id = not_node.execute(self.node_id_to_function_id[node_id])
            not_node_id = self._add_node(not_node, not_node_function_id)
            not_node_ids.append(not_node_id)
            self.edges.append(Multiport3Inputs.Edge(node_id, not_node_id))
        return not_node_ids

    def _add_conjunctions(self, node_ids):
        layer = node_ids
        for _ in range(Multiport3Inputs.INPUT_COUNT - 1):
            layer_new = set()
            for idx, node_id_first in enumerate(layer[:-1]):
                for node_id_second in layer[idx + 1:]:
                    and_node = AndNode()
                    function_id = and_node.execute(self.node_id_to_function_id[node_id_first],
                                                   self.node_id_to_function_id[node_id_second])
                    if function_id not in self.function_id_to_node_id:
                        and_node_id = self._add_node(and_node, function_id)
                        self.edges.append(Multiport3Inputs.Edge(node_id_first, and_node_id))
                        self.edges.append(Multiport3Inputs.Edge(node_id_second, and_node_id))
                        if function_id > 0:
                            layer_new.add(and_node_id)
            layer = list(layer_new)
        return layer

    def _add_disjunctions(self, nodes_single_one):
        layer = set(nodes_single_one)
        for one_count in range(1, Multiport3Inputs.FUNCTION_ID_LENGTH):
            layer_new = set()
            for node_id in layer:
                for node_id_single_one in nodes_single_one:
                    if self.node_id_to_function_id[node_id] & \
                            self.node_id_to_function_id[node_id_single_one] > 0:
                        continue
                    or_node = OrNode()
                    function_id = or_node.execute(self.node_id_to_function_id[node_id],
                                                  self.node_id_to_function_id[node_id_single_one])
                    if function_id not in self.function_id_to_node_id:
                        or_node_id = self._add_node(or_node, function_id)
                        self.edges.append(Multiport3Inputs.Edge(node_id, or_node_id))
                        self.edges.append(Multiport3Inputs.Edge(node_id_single_one, or_node_id))
                    layer_new.add(self.function_id_to_node_id[function_id])
            layer = layer_new
        return layer

    def _add_node(self, node, function_id):
        node_id = len(self.nodes)
        self.nodes[node_id] = node
        self.function_id_to_node_id[function_id] = node_id
        self.node_id_to_function_id[node_id] = function_id
        return node_id

    def get_edges(self):
        return self.edges

    def get_nodes(self):
        return self.nodes


if __name__ == '__main__':
    multiport = Multiport3Inputs()
    for edge in multiport.get_edges():
        print('{} {}'.format(edge.source, edge.target))
    for node_id, node in multiport.get_nodes().items():
        if not isinstance(node, InputNode):
            print('{} {}'.format(node_id, str(node)))
