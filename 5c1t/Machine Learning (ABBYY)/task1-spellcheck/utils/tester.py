from utils.timer import Timer


class SpellcheckTester:
    def test(self, spellchecker, samples):
        timer = Timer()
        timer.start()
        accuracy = sum(1 if spellchecker.correct(wrong) == correct else 0 for wrong, correct in samples) / len(samples)
        time = timer.stop()
        return {'accuracy': accuracy, 'time': time}
