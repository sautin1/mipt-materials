import time


class Timer:
    def __init__(self):
        self.time_start = None

    def start(self):
        self.time_start = time.time()

    def stop(self):
        return time.time() - self.time_start
