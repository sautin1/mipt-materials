import re
import pymorphy2
from collections import Counter


def text_to_wordlist(sentence):
    regexp = "[^а-яА-Яё]"
    sentence = re.sub(regexp, " ", sentence)
    result = sentence.lower().split()
    return result


print('Трудно быть Богом')
with open('htbg.txt', 'r') as fin:
    text = fin.read()

print('1. посчитайте количество слов')
words = text_to_wordlist(text)
print(len(words))
print()

print('2. выведите топ-10 лемм у существительных')
morph_analyzer = pymorphy2.MorphAnalyzer()
morphs = list(map(lambda word: morph_analyzer.parse(word)[0], words))
print('morphs generated')

nouns = filter(lambda morph: 'NOUN' in morph.tag, morphs)
nouns_lemmes = map(lambda noun: noun.normal_form, nouns)
counter = Counter(nouns_lemmes)
print(counter.most_common(10))

print('3. выведите топ-10 лемм у глаголов')
verbs = filter(lambda morph: 'VERB' in morph.tag, morphs)
verbs_lemmes = map(lambda verb: verb.normal_form, verbs)
counter = Counter(verbs_lemmes)
print(counter.most_common(10))
