from barrington import solve, parse_node


class TestFailed(Exception):
    pass


class TestCase:
    def __init__(self, name, var_names, lines, evaluate):
        self._name = name
        self._var_names = var_names
        self._lines = lines
        self._evaluate = evaluate

    def make_input(self):
        return '\n'.join([
            f'{len(self._var_names) + len(self._lines)}',
            *map(lambda var: 'VAR ' + var, self._var_names),
            *self._lines
        ])

    def get_name(self):
        return self._name

    def _decompose_into_bools(self, number):
        result = [False] * len(self._var_names)
        for i in range(len(self._var_names) - 1, -1, -1):
            result[i] = (number & 1) > 0
            number //= 2
        return result

    def check(self, program):
        for x in range(2 ** len(self._var_names)):
            var_values = self._decompose_into_bools(x)
            result_tested = program.evaluate(dict(zip(self._var_names, var_values)))
            result_correct = self._evaluate(*var_values)
            if result_tested != result_correct:
                message = f'Test failed: {dict(zip(self._var_names, var_values))}. ' \
                          f'Program returned {result_tested}, correct is {result_correct}.'
                raise TestFailed(message)


TEST_CASES = [
    TestCase('Input1', ['x'], [], lambda x: x),
    TestCase('Not1', ['x'], ['NOT 0'], lambda x: not x),
    TestCase('And2', ['x', 'y'], ['AND 0 1'], lambda x, y: x and y),
    TestCase('Or2', ['x', 'y'], ['OR 0 1'], lambda x, y: x or y),
    TestCase('Xor2', ['x', 'y'], ['OR 0 1', 'AND 0 1', 'NOT 3', 'AND 2 4'], lambda x, y: x != y),
    TestCase('Majority3', ['x', 'y', 'z'], ['AND 0 1', 'AND 0 2', 'AND 1 2', 'OR 3 4', 'OR 5 6'],
             lambda x, y, z: (x and y) or (x and z) or (y and z))
]


if __name__ == '__main__':
    for test_case in TEST_CASES:
        test_input = test_case.make_input().split('\n')
        node_count = int(test_input[0])
        nodes = [None] * node_count
        node_inputs = [None] * node_count
        for idx, line in enumerate(test_input[1:]):
            nodes[idx], node_inputs[idx] = parse_node(line)
        program = solve(nodes, node_inputs)
        test_case.check(program)
        print(f'Test \'{test_case.get_name()}\' passed!')
    print(f'All tests passed!')
