#include <iostream>
#include <sstream>
#include <string>
#include "annealing.h"

std::string showState(const State& state) {
    std::stringstream sstream;
    sstream << '(';
    for (size_t i = 0; i < state.size(); ++i) {
        sstream << state[i];
        if (i < state.size() - 1) {
            sstream << ", ";
        }
    }
    sstream << ')';
    return sstream.str();
}

double func1(const State& x) {
    return std::pow(2, x[0]) * std::sin(std::pow(2, -x[0]));
}

double func2(const State& x) {
    return (((((6*x[0] + 1) * x[0] + 4) * x[0] + 0) * x[0] - 20) * x[0] + 4) * x[0] + 5;
}

double func3(const State& x) {
//  Rosenbrock function for a = e
    return 100 * std::pow(x[1] - std::pow(x[0], 2), 2) + std::pow(std::exp(1) - x[0], 2);
}

double func4(const State& x) {
//  Bukin function #6
    return 100 * std::sqrt(std::abs(x[1] - 0.01 * std::pow(x[0], 2))) + 0.01 * std::abs(x[0] + 10);
}

double func5(const State& x) {
//  Sphere function
    double res = 0;
    for (size_t i = 0; i < x.size(); ++i) {
        res += std::pow(x[i], 2);
    }
    return res;
}

double func6(const State& x) {
    return std::sin(x[0] + 5) + std::sin(3*x[0] + 8);
}

void test1() {
    // -2.16781
    std::cout << "Test 1\n";
    GoalFunction<double(*)(const State&)> goal_func(func1, std::make_pair(State {-10},
                                                                   State {3}));
    Annealing<double(*)(const State&)> test(goal_func, Temperature(1, 1e-8), Temperature(1, 1e2));
    State result = test.annealMin(State {0.0});
    std::cout << showState(result) << '\n';
}

void test2() {
    // -0.981376
    std::cout << "Test 2\n";
    GoalFunction<double(*)(const State&)> goal_func(func2, std::make_pair(State {-3},
                                                                   State {10}));
    Annealing<double(*)(const State&)> test(goal_func, Temperature(1, 1e-8), Temperature(1, 1e2));
    State result = test.annealMin(State {0.0});
    std::cout << showState(result) << '\n';
}

void test3() {
    // (e, e^2)
    std::cout << "Test 3\n";
    GoalFunction<double(*)(const State&)> goal_func(func3, std::make_pair(State {0, 0},
                                                                   State {5, 10}));
    Annealing<double(*)(const State&)> test(goal_func, Temperature(2, 1e-8), Temperature(2, 1e5));
    State result = test.annealMin(State {2.0, 7.2});
    std::cout << showState(result) << '\n';
}

void test4() {
    // (-10, 1)
    std::cout << "Test 4\n";
    GoalFunction<double(*)(const State&)> goal_func(func4, std::make_pair(State {-15, 0},
                                                                   State {-5, 4}));
    Annealing<double(*)(const State&)> test(goal_func, Temperature(2, 1e-4), Temperature(2, 1e7));
    State result = test.annealMin(State {-13.0, 0.2});
    std::cout << showState(result) << '\n';
}

void test5() {
    // (0, 0, 0, 0, 0)
    std::cout << "Test 5\n";
    GoalFunction<double(*)(const State&)> goal_func(func5, std::make_pair(State {-10, -10, -10, -10, -10},
                                                                   State { 10,  10,  10,  10,  10}));
    Annealing<double(*)(const State&)> test(goal_func, Temperature(5, 1e-8), Temperature(5, 1e9));
    State result = test.annealMin(State {-8.0, 7.0, -3.0, -0.2, -8.5});
    std::cout << showState(result) << '\n';
}

void test6() {
    // (5.26231)
    std::cout << "Test 6\n";
    GoalFunction<double(*)(const State&)> goal_func(func6, std::make_pair(State {-10},
                                                                   State {10}));
    Annealing<double(*)(const State&)> test(goal_func, Temperature(1, 1e-8), Temperature(1, 1e2));
    State result = test.annealMin(State {0.0});
    std::cout << showState(result) << '\n';
}

struct SinGoalFunction {
    int a, b, c, d;
    SinGoalFunction(int _a, int _b, int _c, int _d)
        : a(_a), b(_b), c(_c), d(_d) {}

    double operator() (const State& x) const {
        return 1.0/a * std::sin(a*x[0] + b) + 1.0/c * std::sin(c*x[0] + d);
    }
};

int gcd(int x, int y) {
    while (x != 0) {
        double tmp = x;
        x = y % x;
        y = tmp;
    }
    return y;
}

const double pi = 3.14159265358979323846264338327950288419716939937510582097494;

double sinRandRealAns(const GoalFunction<SinGoalFunction>& goal_func) {
    SinGoalFunction f = goal_func.func();
    double res1_x = (pi - f.b - f.d) / (f.a + f.c);
    double tmp1 = (f.b + f.d - pi) / (2 * pi);
    int k1 = std::ceil(std::abs(tmp1));
    if (tmp1 < 0) {
        k1 *= -1;
    }
    double step1 = std::abs((2 * pi) / (f.a + f.c));

    res1_x += step1 * k1;
    double min1_x = res1_x;
    double min1_y = f(State {res1_x});
    for (; res1_x <= goal_func.bounds().second.front(); res1_x += step1) {
        double res1_y = f(State {res1_x});
        if (res1_y < min1_y) {
            min1_x = res1_x;
            min1_y = res1_y;
        }
    }

    double res2_x = (pi + f.d - f.b) / (f.a - f.c);
    double tmp2 = (f.b - f.d - pi) / (2 * pi);
    int k2 = std::ceil(std::abs(tmp2));
    if (tmp2 < 0) {
        k2 *= -1;
    }
    double step2 = std::abs((2 * pi) / (f.a - f.c));

    res2_x += step2 * k2;
    double min2_x = res2_x;
    double min2_y = f(State {res2_x});
    for (; res2_x <= goal_func.bounds().second.front(); res2_x += step2) {
        double res2_y = f(State {res2_x});
        if (res2_y < min2_y) {
            min2_x = res2_x;
            min2_y = res2_y;
        }
    }

    if (min1_y < min2_y) {
        return min1_x;
    } else {
        return min2_x;
    }
}

double testSinRand() {
    const int max_coef = 1000;
    int a = rand() % (max_coef - 1) + 1;
    int b = rand() % max_coef;
    int c;
    do {
        c = rand() % (max_coef - 1) + 1;
    } while (c == a);
    int d = rand() % max_coef;
    double period = 2 * pi / gcd(a, c);
    GoalFunction<SinGoalFunction> goal_func(SinGoalFunction(a, b, c, d), std::make_pair(State {0},
                                                                                        State {period}));
    Annealing<SinGoalFunction> test(goal_func, Temperature(1, 1e-8), Temperature(1, 1e2));
    double res_ann = test.annealMin(State {5.0}).front();
    double res_corr = sinRandRealAns(goal_func);
    double rel_err = std::abs(res_ann - res_corr) / res_corr;
//  std::cout << "(" << res_ann << ", " << res_corr << "). RelErr = " << rel_err << "\n";
    return rel_err;
}

int getStats(const std::vector<double>& errs, double acceptable_rel_err) {
    int counter = 0;
    for (size_t i = 0; i < errs.size(); ++i) {
        if (errs[i] <= acceptable_rel_err) {
            ++counter;
        }
    }
    return counter;
}

void launchTests() {
    // individual tests
    std::cout << "Individual tests\n";
    test1(); // correct
    test2(); // correct
    test3(); // helds up in the valley
    test4(); // helds up in a local minimum
    test5(); // correct
    test6(); // correct

    // group tests: testing work with functions of sin family
    std::cout << "Group tests\n";
    const int test_q = 10000;
    const double acceptable_rel_err = 0.30;

    std::vector<double> errs;
    errs.reserve(test_q);
    for (int i = 0; i < test_q; ++i) {
        errs.push_back(testSinRand());
        if (i % 100 == 0) {
            std::cout << i / 100 << "\n";
        }
    }
    int stats = getStats(errs, acceptable_rel_err);
    std::cout << stats << " / " << errs.size() << " = " << stats * 100.0 / errs.size() << "% of tests passed\n";
    std::cout << "(A test is passed if the algorithm produces relative error of at most " << acceptable_rel_err * 100 << "%)\n";
}

int main() {
    std::srand(std::time(0));
    launchTests();
    return 0;
}
