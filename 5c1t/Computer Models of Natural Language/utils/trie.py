class Trie:

    class Node:
        count = 0

        def __init__(self, is_terminal, parent=-1, parent_edge=None):
            self.transitions = {}
            self.is_terminal = is_terminal
            self.id = Trie.Node.count
            self.parent = parent
            self.parent_edge = parent_edge
            Trie.Node.count += 1

    def __init__(self, words):
        self.root = Trie.Node(False)
        self.nodes = {
            self.root.id: self.root
        }
        self.extend(words)

    def add_word(self, word):
        if not word:
            self.root.is_terminal = True
            return

        node = self.root.id
        for idx, letter in enumerate(word):
            if letter not in self.nodes[node].transitions:
                new_node = Trie.Node(idx == len(word) - 1, node, letter)
                self.nodes[node].transitions[letter] = new_node.id
                self.nodes[new_node.id] = new_node
            node = self.nodes[node].transitions[letter]

    def extend(self, words):
        for word in words:
            self.add_word(word)

    def is_terminal(self, node):
        return self.nodes[node].is_terminal

    def get_transitions(self, node):
        return self.nodes[node].transitions

    def get_node_string(self, node):
        string = ''
        while node > 0:
            string += self.nodes[node].parent_edge
            node = self.nodes[node].parent
        return string[::-1]
