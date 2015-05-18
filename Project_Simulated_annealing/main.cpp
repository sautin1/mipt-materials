#include <iostream>
#include "annealing.h"

double func1(double x) {
    return std::pow(2, x) * std::sin(std::pow(2, -x));
}

int main() {
    GoalFunction<double(*)(double)> goal_func1(func1, std::make_pair(-10, 3));
    Annealing<double(*)(double)> test1(goal_func1, 1e-8, 100);
    std::cout << test1.annealMin(0.0) << '\n';
    return 0;
}

