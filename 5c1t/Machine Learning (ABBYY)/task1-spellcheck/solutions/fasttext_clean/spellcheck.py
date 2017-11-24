from gensim.models import KeyedVectors
from os.path import join

from utils.database import read_vocabulary
from utils.tester import test_on_datasets
from utils.paths import PATH_PROJECT


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


PATH_MODEL = join(PATH_PROJECT, 'models/wiki-news-300d-1M-subword.vec')
PATH_VOCABULARY = join(PATH_PROJECT, 'data/big.txt')
PATHS_DATASETS = list(map(lambda suffix: join(PATH_PROJECT, suffix),
                          ['data/spell-testset1.txt', 'data/spell-testset2.txt']))


model = KeyedVectors.load_word2vec_format(PATH_MODEL, limit=999999)
vocabulary = read_vocabulary(PATH_VOCABULARY)
spellchecker = SpellCheckerFastTextSimilarity(model, vocabulary, 20)

test_on_datasets(spellchecker, PATHS_DATASETS)
