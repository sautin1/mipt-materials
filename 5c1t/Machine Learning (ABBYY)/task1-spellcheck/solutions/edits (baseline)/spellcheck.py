from spellchecking.edits import SpellCheckerSimpleEdits
from utils.database import read_vocabulary
from utils.tester import test_on_datasets


spellchecker = SpellCheckerSimpleEdits(read_vocabulary('data/big.txt'))
test_on_datasets(spellchecker, ['data/spell-testset1.txt', 'data/spell-testset2.txt'])
