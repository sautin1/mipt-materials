from collections import namedtuple


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

    Config = namedtuple('Config', ['state', 'needle_index'])

    def __init__(self, words):
        self.root = Trie.Node(False)
        self.nodes = {
            self.root.id: self.root
        }
        self.extend(words)
        self.config_to_distance = {}

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

    def _get_node_string(self, node):
        string = ''
        while node > 0:
            string += self.nodes[node].parent_edge
            node = self.nodes[node].parent
        return string[::-1]

    def _update_distance_to_config(self, config, new_distance, max_acceptable_distance):
        result = False
        if new_distance <= max_acceptable_distance:
            distance_previous = self.config_to_distance.get(config,
                                                            max_acceptable_distance + 1)
            if new_distance < distance_previous:
                self.config_to_distance[config] = new_distance
                result = True
        return result

    def _produce_new_configs(self, needle, config, max_acceptable_distance):
        node, needle_index = config
        distance = self.config_to_distance[config]
        configs_new = set()
        if needle_index < len(needle):
            # can remove a letter
            config_remove_letter = Trie.Config(node, needle_index + 1)
            is_added = self._update_distance_to_config(config_remove_letter,
                                                       distance + 1,
                                                       max_acceptable_distance)
            if is_added:
                configs_new.add(config_remove_letter)
        for letter, target in self.nodes[node].transitions.items():
            # add a letter
            config_add_letter = Trie.Config(target, needle_index)
            is_added = self._update_distance_to_config(config_add_letter,
                                                       distance + 1,
                                                       max_acceptable_distance)
            if is_added:
                configs_new.add(config_add_letter)
            if needle_index < len(needle):
                # replace a letter
                replace_cost = 0 if needle[needle_index] == letter else 1
                config_replace_letter = Trie.Config(target, needle_index + 1)
                is_added = self._update_distance_to_config(config_replace_letter,
                                                           distance + replace_cost,
                                                           max_acceptable_distance)
                if is_added:
                    configs_new.add(config_replace_letter)
            if needle_index + 1 < len(needle):
                transpose_target = self.nodes[target].transitions.get(needle[needle_index], None)
                if needle[needle_index + 1] == letter and transpose_target is not None:
                    # transpose letters
                    config_transpose = Trie.Config(transpose_target, needle_index + 2)
                    is_added = self._update_distance_to_config(config_transpose,
                                                               distance + 1,
                                                               max_acceptable_distance)
                    if is_added:
                        configs_new.add(config_transpose)

        return configs_new

    def _update_closest_nodes(self, closest_nodes, config):
        if self.config_to_distance[config] < closest_nodes['distance']:
            closest_nodes['nodes'] = [config.state]
            closest_nodes['distance'] = self.config_to_distance[config]
        elif self.config_to_distance[config] == closest_nodes['distance']:
            closest_nodes['nodes'].append(config.state)

    def find_closest_words(self, needle, max_acceptable_distance, verbose=False):
        config_initial = Trie.Config(0, 0)
        self.config_to_distance[config_initial] = 0
        configs = {config_initial}
        closest_nodes = {
            'distance': max_acceptable_distance + 1,
            'nodes': []
        }
        while configs:
            if verbose:
                print(f'Iter. Configs ({len(configs)}): {{', '; '.join(map(
                    lambda conf: '(' + str(conf.state) + ', ' + str(conf.needle_index) + ')',
                    configs)) + ' }')
            configs_new = set()
            for config in configs:
                node, needle_index = config
                if self.nodes[node].is_terminal and needle_index == len(needle):
                    self._update_closest_nodes(closest_nodes, config)
                configs_produced = self._produce_new_configs(needle, config,
                                                             max_acceptable_distance)
                configs_new.update(configs_produced)
            configs = configs_new
        return {
            'distance': closest_nodes['distance'],
            'words': list(map(self._get_node_string, closest_nodes['nodes']))
        }
