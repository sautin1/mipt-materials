#include <iostream>
#include "binomial_heap.h"

int main()
{
    binomial_heap heap;
    heap.insert(0.5);
    heap.insert(0.9);
    std::shared_ptr<node> third = heap.insert(2.6);
    heap.insert(0.2);
    heap.insert(1.2);
    std::shared_ptr<node> fifth = heap.insert(5.2);
    std::cout << heap.extract_min() << "\n";
    /*std::cout << heap.extract_min() << "\n";
    std::cout << heap.extract_min() << "\n";
    std::cout << heap.extract_min() << "\n";
    std::cout << heap.extract_min() << "\n";
    std::cout << heap.extract_min() << "\n";*/
    heap.change_key(third, 0.6);
    std::cout << heap.extract_min() << "\n";
    std::cout << heap.get_min() << "\n";
    heap.change_key(fifth, 0.4);
    std::cout << heap.get_min() << "\n";
    heap.change_key(fifth, 2.0);
    std::cout << heap.get_min() << "\n";
    return 0;
}
