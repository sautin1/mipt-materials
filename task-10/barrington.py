from circuit import NotNode, AndNode, OrNode, InputNode, LogicalCircuit
from branching_program import PermutingBranchingProgram


def parse_node(node_str):
    node_description = node_str.split()
    if node_description[0] == 'VAR':
        node = InputNode(node_description[1])
        inputs = None
    elif node_description[0] == 'OR':
        node = OrNode()
        inputs = list(map(int, node_description[1:]))
    elif node_description[0] == 'AND':
        node = AndNode()
        inputs = list(map(int, node_description[1:]))
    elif node_description[0] == 'NOT':
        node = NotNode()
        inputs = list(map(int, node_description[1:]))
    else:
        raise ValueError('Wrong node type in input')
    return node, inputs


if __name__ == '__main__':
    node_count = int(input())
    node_inputs = {}
    nodes = {}
    for idx in range(node_count):
        nodes[idx], node_inputs[idx] = parse_node(input())

    circuit = LogicalCircuit(nodes, node_inputs, node_count - 1).eliminate_or_nodes()
    permuting_program = PermutingBranchingProgram.build_from_circuit(circuit)
    print('Success')
