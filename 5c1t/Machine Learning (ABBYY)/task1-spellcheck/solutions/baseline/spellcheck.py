from os.path import join

from spellchecking import SpellChecker
from utils.database import read_vocabulary
from utils.paths import PATH_PROJECT
from utils.tester import test_on_datasets

PATH_VOCABULARY = join(PATH_PROJECT, 'data/big.txt')
PATHS_DATASETS = list(map(lambda suffix: join(PATH_PROJECT, suffix),
                          ['data/spell-testset1.txt', 'data/spell-testset2.txt']))

vocabulary = read_vocabulary(PATH_VOCABULARY)
spellchecker = SpellChecker(vocabulary)
test_on_datasets(spellchecker, PATHS_DATASETS)
