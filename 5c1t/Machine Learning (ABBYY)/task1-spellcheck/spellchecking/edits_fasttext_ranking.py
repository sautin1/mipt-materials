"""Based on the article by Peter Norvig:
http://norvig.com/spell-correct.html
"""

from string import ascii_lowercase as LETTERS_LOWERCASE


class SpellCheckerEditsWithFastTextRanking:
    def __init__(self, model):
        words = model.index2word
        self.vocabulary = {word: idx for idx, word in enumerate(words)}

    def filter_words_by_vocabulary(self, words):
        return set(word for word in words if word in self.vocabulary)

    def generate_spelling_candidates(self, word):
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
        return max(candidates, key=lambda candidate: -self.vocabulary.get(candidate, 0), default=word)
