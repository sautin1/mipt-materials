"""Based on the article by Peter Norvig:
http://norvig.com/spell-correct.html
"""

import re
from string import ascii_lowercase as LETTERS_LOWERCASE
from collections import Counter


class SpellCheckerSimpleEdits:
    def __init__(self, vocabulary_path):
        with open(vocabulary_path, 'r') as fin:
            words = re.findall(r'\w+', fin.read().lower())
        word_counter = Counter(words)
        self.frequencies = {word: word_counter[word] / len(words) for word in word_counter}

    def filter_words_by_vocabulary(self, words):
        return set(word for word in words if word in self.frequencies)

    def generate_spelling_candidates(self, word):
        if word in self.frequencies:
            return {word}
        edits = self.generate_edits(word)
        result = self.filter_words_by_vocabulary(edits)
        if not result:
            edits = (edit2 for edit in edits for edit2 in self.generate_edits(edit))
            result = self.filter_words_by_vocabulary(edits)
        return result

    def generate_edits(self, word):
        splits = [(word[:i], word[i:]) for i in range(len(word) + 1)]
        deletes = {prefix + suffix[1:] for prefix, suffix in splits if suffix}
        transposes = {prefix + suffix[1] + suffix[0] + suffix[2:]
                      for prefix, suffix in splits if len(suffix) > 1}
        replaces = {prefix + letter + suffix[1:] for prefix, suffix in splits for letter in LETTERS_LOWERCASE}
        inserts = {prefix + letter + suffix for prefix, suffix in splits for letter in LETTERS_LOWERCASE}
        return deletes | transposes | replaces | inserts

    def correct(self, word):
        candidates = self.generate_spelling_candidates(word)
        return max(candidates, key=lambda candidate: self.frequencies[candidate], default=word)


if __name__ == '__main__':
    from utils.database import BirkbeckCorpusReader
    from utils.tester import SpellcheckTester

    spellchecker = SpellCheckerSimpleEdits('data/big.txt')
    for path in ['data/spell-testset1.txt', 'data/spell-testset2.txt']:
        database = BirkbeckCorpusReader(path)
        results = SpellcheckTester().test(spellchecker, database)
        print(f'base: {path}')
        print(f'words count: {len(database)}')
        for key, value in results.items():
            print(f'{key}: {value}')
        print('-' * 5)
