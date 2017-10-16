import re
from os.path import join
from collections import Counter, deque


def text_to_wordlist(sentence):
    regexp = "[^а-яА-Яё]"
    sentence = re.sub(regexp, " ", sentence)
    result = sentence.lower().split()
    return result


def sliding_window(iterable, width):
    window = deque()
    for x in iterable:
        window.append(x)
        if len(window) > width:
            window.popleft()
        if len(window) == width:
            yield window


def generate_ngrams(word, n=3):
    blanks = '#' * (n - 1)
    word_extended = blanks + word + blanks
    return map(lambda letters: ''.join(letters), sliding_window(word_extended, n))


def count_ngrams(words, n=3, is_relative=False):
    counter = Counter()
    for word in words:
        counter.update(generate_ngrams(word, n))
    result = counter
    if is_relative:
        counter_sum = sum(counter.values())
        print(counter_sum)
        result = {key: value / counter_sum for key, value in counter.items()}
    return result


def has_typo(word, ngrams_frequency, threshold, n=None, verbose=False):
    if n is None:
        for key in ngrams_frequency.keys():
            n = len(key)
            break
    ngrams = generate_ngrams(word, n)

    result = False
    for ngram in ngrams:
        frequency = ngrams_frequency.get(ngram, 0)
        if verbose:
            print(f'{ngram}: {frequency}')
        result = result or frequency < threshold
        if result:
            break
    return result


with open(join('..', 'data', 'wp.txt')) as fin:
    text = fin.read()

words = text_to_wordlist(text)
ngrams_frequency = count_ngrams(words, 3, is_relative=True)

typo = has_typo('ашипка', ngrams_frequency, 1.0e-5, verbose=True)
print(typo)
