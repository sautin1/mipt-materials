from circuit import NotNode, AndNode, OrNode, InputNode, LogicalCircuit
from branching_program import PermutingBranchingProgram, BranchingProgram


def parse_node(node_str):
    node_description = node_str.split()
    inputs_start_idx = 1
    if node_description[0] == 'VAR':
        node = InputNode(node_description[1])
        inputs_start_idx = 2
    elif node_description[0] == 'OR':
        node = OrNode()
    elif node_description[0] == 'AND':
        node = AndNode()
    elif node_description[0] == 'NOT':
        node = NotNode()
    else:
        raise ValueError('Wrong node type in input')
    inputs = list(map(int, node_description[inputs_start_idx:]))
    return node, inputs


if __name__ == '__main__':
    node_count = int(input())
    nodes_inputs = [None] * node_count
    nodes = [None] * node_count
    for idx in range(node_count):
        nodes[idx], nodes_inputs[idx] = parse_node(input())

    circuit = LogicalCircuit(nodes, nodes_inputs, node_count - 1, eliminate_or_nodes=True)
    permuting_program = PermutingBranchingProgram.build_from_circuit(circuit)
    branching_program = BranchingProgram.build_from_permuting_branching_program(permuting_program)

    graph, labels = branching_program.get_graph(), branching_program.get_labels()
    for node in range(len(graph)):
        label = labels[node]
        if isinstance(label, bool):
            label = str(label).upper()
            print(f'{label}')
        else:
            print(f'{label} {graph[node][False]} {graph[node][True]}')
