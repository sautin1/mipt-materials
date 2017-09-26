from collections import namedtuple


class FuzzySearcher:
    Config = namedtuple('Config', ['state', 'needle_index'])

    def __init__(self, trie, max_distance):
        self.trie = trie
        self.max_distance = max_distance

        self.config_to_distance = None
        self.closest_nodes = None

    def _add_config(self, config, distance, config_set):
        is_added = False
        if distance <= self.max_distance:
            distance_old = self.config_to_distance.get(config, self.max_distance + 1)
            if distance < distance_old:
                self.config_to_distance[config] = distance
                config_set.add(config)
                is_added = True
        return is_added

    def _produce_new_configs(self, needle, config):
        configs = set()
        node, needle_index = config
        distance = self.config_to_distance[config]
        if needle_index < len(needle):
            # can remove a letter
            self._add_config(FuzzySearcher.Config(node, needle_index + 1), distance + 1, configs)
        for letter, target in self.trie.get_transitions(node).items():
            # add a letter
            self._add_config(FuzzySearcher.Config(target, needle_index), distance + 1, configs)
            if needle_index < len(needle):
                # replace a letter
                replace_cost = 0 if needle[needle_index] == letter else 1
                self._add_config(FuzzySearcher.Config(target, needle_index + 1),
                                 distance + replace_cost, configs)
            if needle_index + 1 < len(needle):
                transpose_target = self.trie.get_transitions(target)\
                    .get(needle[needle_index], None)
                if needle[needle_index + 1] == letter and transpose_target is not None:
                    # transpose letters
                    self._add_config(FuzzySearcher.Config(transpose_target, needle_index + 2),
                                     distance + 1, configs)
        return configs

    def _register_result(self, config):
        if self.config_to_distance[config] < self.closest_nodes['distance']:
            self.closest_nodes['nodes'] = {config.state}
            self.closest_nodes['distance'] = self.config_to_distance[config]
        elif self.config_to_distance[config] == self.closest_nodes['distance']:
            self.closest_nodes['nodes'].add(config.state)

    def search(self, needle, verbose=False):
        configs = set()
        self.config_to_distance = {}
        self.closest_nodes = {
            'distance': self.max_distance + 1,
            'nodes': set()
        }
        self._add_config(FuzzySearcher.Config(0, 0), 0, configs)
        while configs:
            if verbose:
                print(f'Iter. Configs ({len(configs)}): {{', '; '.join(map(
                    lambda conf: '(' + str(conf.state) + ', ' + str(conf.needle_index) + ')',
                    configs)) + ' }')
            configs_new = set()
            for config in configs:
                node, needle_index = config
                if self.trie.is_terminal(node) and needle_index == len(needle):
                    self._register_result(config)
                configs_produced = self._produce_new_configs(needle, config)
                configs_new.update(configs_produced)
            configs = configs_new
        return {
            'distance': self.closest_nodes['distance'],
            'words': list(map(self.trie.get_node_string, self.closest_nodes['nodes']))
        }
