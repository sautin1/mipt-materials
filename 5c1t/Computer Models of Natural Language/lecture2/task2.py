from os.path import join
from utils.tokenization import split_into_words
from utils.trie import Trie
from utils.fuzzy_search import FuzzySearcher


with open(join('..', 'data', 'wp.txt')) as fin:
    text = fin.read()

words = split_into_words(text)

# words = ['мама', 'папа']
trie = Trie(words)
searcher = FuzzySearcher(trie, 2)

needle = 'ашипка'  # 'ама'
result = searcher.search(needle, verbose=True)
print(result)
