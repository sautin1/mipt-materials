from gensim.models import KeyedVectors
from os.path import join

from spellchecking.edits_fasttext_lang_model import SpellCheckerSimpleEditsWithFastTextLangModel
from utils.database import read_vocabulary
from utils.tester import test_on_datasets
from utils.paths import PATH_PROJECT

PATH_MODEL = join(PATH_PROJECT, 'models/wiki-news-300d-1M-subword.vec')
PATH_VOCABULARY = join(PATH_PROJECT, 'data/big.txt')
PATHS_DATASETS = list(map(lambda suffix: join(PATH_PROJECT, suffix),
                          ['data/spell-testset1.txt', 'data/spell-testset2.txt']))


model = KeyedVectors.load_word2vec_format(PATH_MODEL, limit=999999)
vocabulary = read_vocabulary(PATH_VOCABULARY)
spellchecker = SpellCheckerSimpleEditsWithFastTextLangModel(model)

test_on_datasets(spellchecker, PATHS_DATASETS)
