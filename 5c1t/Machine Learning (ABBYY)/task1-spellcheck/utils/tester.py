from utils.timer import Timer
from utils.database import BirkbeckCorpusReader


def test_on_datasets(spellchecker, datasets):
    for path in datasets:
        database = BirkbeckCorpusReader(path)
        results = SpellcheckTester().test(spellchecker, database)
        print(f'base: {path}')
        print(f'words count: {len(database)}')
        for key, value in results.items():
            print(f'{key}: {value}')
        print('-' * 5)


class SpellcheckTester:
    def test(self, spellchecker, samples):
        timer = Timer()
        timer.start()
        accuracy = sum(1 if spellchecker.correct(wrong) == correct else 0 for wrong, correct in samples) / len(samples)
        time = timer.stop()
        return {'accuracy': accuracy, 'time': time}
