#include <chrono>
#include <iostream>
#include "async.h"

int main() {
    std::shared_ptr<CPromise<int>> promise(new CPromise<int>());
    CFuture<int> future = promise->GetFuture();

    auto work = [promise](int workDuration) {
        std::cout << "Slave: working" << std::endl;
        std::this_thread::sleep_for(std::chrono::microseconds(workDuration));
        promise->SetValue(23);
    };
    std::thread thread(work, 1000);
    thread.detach();

//    std::cout << future.TryGet() << std::endl;
    std::cout << "Master: Waiting" << std::endl;
    int result = *(future.Get());
    std::cout << result << std::endl;
    return 0;
}
