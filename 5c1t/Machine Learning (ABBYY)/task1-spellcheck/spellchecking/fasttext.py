class SpellCheckerFastTextSimilarity:
    def __init__(self, fasttext_model, vocabulary, similar_count=10):
        self.fasttext_model = fasttext_model
        self.similar_count = similar_count
        self.vocabulary = vocabulary

    def correct(self, word):
        result = None
        if word in self.fasttext_model.wv.vocab:
            distances = dict(self.fasttext_model.similar_by_word(word, topn=self.similar_count))
            candidates = {word for word in distances if word in self.vocabulary}
            result = max(candidates, key=lambda candidate: distances[candidate] * self.vocabulary[candidate],
                         default=None)
        return result or word
