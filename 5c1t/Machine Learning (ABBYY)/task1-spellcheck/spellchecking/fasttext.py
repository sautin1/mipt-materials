class SpellCheckerFastTextSimilarity:
    def __init__(self, fasttext_model, vocabulary, similar_count=10):
        self.fasttext_model = fasttext_model
        self.similar_count = similar_count
        self.vocabulary = vocabulary

    def correct(self, word):
        result = None
        print(word)
        if word in self.fasttext_model.wv.vocab:
            similar_words = self.fasttext_model.similar_by_word(word, topn=self.similar_count)
            print(similar_words)
            distances = dict(similar_words)
            print(distances)
            candidates = {word for word in similar_words if word in self.vocabulary}
            print(candidates)
            result = max(candidates, key=lambda candidate: distances[candidate] * self.vocabulary[candidate],
                         default=None)
            print(result)
        print('-' * 10)
        _ = input()
        return result or word
