import re


def split_into_words(sentence):
    regexp = "[^а-яА-Яё]"
    sentence = re.sub(regexp, " ", sentence)
    result = sentence.lower().split()
    return result
