"""Based on the article by Peter Norvig:
http://norvig.com/spell-correct.html
"""

from string import ascii_lowercase as LETTERS_LOWERCASE


class SpellCheckerEditsWithFastTextSimilarity:
    def __init__(self, vocabulary, model):
        self._vocabulary = {word: vocabulary[word] / len(vocabulary) for word in vocabulary}
        self._model = model

    def filter_words_by_vocabulary(self, words):
        return set(word for word in words if word in self._vocabulary)

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

    def get_word_similarity(self, word_base, word_similar):
        result = 0
        if word_similar in self._model.vocab:
            result = self._model.similarity(word_base, word_similar)
        return result

    def correct(self, word):
        candidates = self.generate_spelling_candidates(word)
        result = None
        if word in self._model.vocab:
            result = max(candidates, key=lambda candidate: self.get_word_similarity(word, candidate), default=None)

        if result is None:
            result = max(candidates, key=lambda candidate: self._vocabulary[candidate], default=word)
        return result