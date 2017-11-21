import itertools


class BirkbeckCorpusReader:
    def __init__(self, base_path):
        with open(base_path, 'r') as fin:
            lines = fin.read().splitlines()
        self._dataset = [line.split(': ') for line in lines]
        self._dataset = [(correct, words.split(' ')) for correct, words in self._dataset]
        self._length = sum(1 for _, words_wrong in self._dataset for _ in words_wrong)

    def __len__(self):
        return self._length

    def __iter__(self):
        test_cases = ((wrong, correct) for correct, wrongs in self._dataset for wrong in wrongs)
        return itertools.chain(test_cases)


if __name__ == '__main__':
    tester = BirkbeckCorpusReader('data/spell-testset1.txt')
    print(list(itertools.islice(tester, 10)))
