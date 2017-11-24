from os.path import join

from gensim.models import KeyedVectors

from spellchecking import SpellChecker
from utils.database import read_vocabulary
from utils.paths import PATH_PROJECT
from utils.tester import test_on_datasets


class SpellCheckerWithFastTextSimilarity(SpellChecker):
    def __init__(self, vocabulary, model):
        super().__init__(vocabulary)
        self._model = model

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


PATH_MODEL = join(PATH_PROJECT, 'models/wiki-news-300d-1M-subword.vec')
PATH_VOCABULARY = join(PATH_PROJECT, 'data/big.txt')
PATHS_DATASETS = list(map(lambda suffix: join(PATH_PROJECT, suffix),
                          ['data/spell-testset1.txt', 'data/spell-testset2.txt']))

model = KeyedVectors.load_word2vec_format(PATH_MODEL, limit=999999)
vocabulary = read_vocabulary(PATH_VOCABULARY)
spellchecker = SpellCheckerWithFastTextSimilarity(vocabulary, model)

test_on_datasets(spellchecker, PATHS_DATASETS)
