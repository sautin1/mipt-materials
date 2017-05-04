#pragma once

#include <semaphore.h>

class CSemaphore {
public:
    explicit CSemaphore(int value = 0) {
        sem_init(&semaphore, 0, value);
    }

    ~CSemaphore() {
        sem_destroy(&semaphore);
    }

    void Post() {
        sem_post(&semaphore);
    }

    void Wait() {
        sem_wait(&semaphore);
    }

    bool TryWait() {
        return sem_trywait(&semaphore) == 0;
    }

    int GetValue() {
        int res = 0;
        sem_getvalue(&semaphore, &res);
        return res;
    }

private:
    sem_t semaphore;
};
