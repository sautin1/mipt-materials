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


class Multiport2Inputs:
    INPUT_COUNT = 2
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
        # add inputs
        input_function_ids = [0b1100, 0b1010]  # ##
        input_node_ids = []
        for i, function_id in enumerate(input_function_ids):
            input_node_id = self._add_node(InputNode(), function_id)
            input_node_ids.append(input_node_id)

        # add input negations
        for input_node_id in input_node_ids:
            not_node = NotNode(Multiport2Inputs.MAX_FUNCTION_ID)
            not_node_function_id = not_node.execute(self.node_id_to_function_id[input_node_id])
            not_node_id = self._add_node(not_node, not_node_function_id)
            self.edges.append(Multiport2Inputs.Edge(input_node_id, not_node_id))
        
        # add and's
        nodes_single_one = set()
        node_ids = list(self.nodes.keys())
        for node_id_first in node_ids[:-1]:
            for node_id_second in node_ids[node_id_first + 1:]:
                and_node = AndNode()
                function_id = and_node.execute(self.node_id_to_function_id[node_id_first],
                                               self.node_id_to_function_id[node_id_second])
                if function_id not in self.function_id_to_node_id:
                    and_node_id = self._add_node(and_node, function_id)
                    self.edges.append(Multiport2Inputs.Edge(node_id_first, and_node_id))
                    self.edges.append(Multiport2Inputs.Edge(node_id_second, and_node_id))
                    if function_id > 0:
                        nodes_single_one.add(and_node_id)

        # add or's
        layer = set(nodes_single_one)
        for one_count in range(1, Multiport2Inputs.FUNCTION_ID_LENGTH):
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
                        self.edges.append(Multiport2Inputs.Edge(node_id, or_node_id))
                        self.edges.append(Multiport2Inputs.Edge(node_id_single_one, or_node_id))
                    layer_new.add(self.function_id_to_node_id[function_id])
            layer = layer_new

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
    multiport = Multiport2Inputs()
    for edge in multiport.get_edges():
        print('{} {}'.format(edge.source, edge.target))
    for node_id, node in multiport.get_nodes().items():
        if not isinstance(node, InputNode):
            print('{} {}'.format(node_id, str(node)))
