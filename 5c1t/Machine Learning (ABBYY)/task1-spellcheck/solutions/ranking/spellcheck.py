from os.path import join

from gensim.models import KeyedVectors

from spellchecking import SpellChecker
from utils.paths import PATH_PROJECT
from utils.tester import test_on_datasets


class SpellCheckerWithFastTextRanking(SpellChecker):
    def __init__(self, model):
        words = model.index2word
        vocabulary = {word: idx for idx, word in enumerate(words)}
        super().__init__(vocabulary)

    def correct(self, word):
        candidates = self.generate_spelling_candidates(word, False)
        return max(candidates, key=lambda candidate: 1 - self._vocabulary.get(candidate, 0), default=word)


PATH_MODEL = join(PATH_PROJECT, 'models/wiki-news-300d-1M-subword.vec')
PATHS_DATASETS = list(map(lambda suffix: join(PATH_PROJECT, suffix),
                          ['data/spell-testset1.txt', 'data/spell-testset2.txt']))

model = KeyedVectors.load_word2vec_format(PATH_MODEL, limit=999999)
spellchecker = SpellCheckerWithFastTextRanking(model)

test_on_datasets(spellchecker, PATHS_DATASETS)
