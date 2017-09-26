from os.path import join
from utils.tokenization import split_into_words
from utils.trie import Trie


with open(join('..', 'data', 'wp.txt')) as fin:
    text = fin.read()

words = split_into_words(text)

trie = Trie(words)

result = trie.find_closest_words('иль', 2, verbose=True)
print(result)
