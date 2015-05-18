#pragma once
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <utility>

using Temperature = double;
using State = double;

template <typename Function>
class GoalFunction {
public:
    using StateBounds = std::pair<State, State>;

    GoalFunction(Function func, const StateBounds& state_bounds)
        : func_(func), bounds_(state_bounds) {}

    double apply(const State& state) const {
        return func_(state);
    }

    std::pair<State, State> bounds() const {
        return bounds_;
    }
private:
    Function func_;
    std::pair<State, State> bounds_;
};

template <typename Function>
class Annealing {
public:
    using StateBounds = std::pair<State, State>;
    using Range = State;

    Annealing(const GoalFunction<Function>& energy_f, Temperature t_min, Temperature t_max)
        : energy_fun_(energy_f), t_min_(t_min), t_max_(t_max) {
        std::srand(std::time(0));
    }

    State annealMin(const State& st_init) const {
        Temperature temp_cur = t_max_;
        State st_cur = st_init;
        State st_best = st_cur;
        double en_best = energy_fun_.apply(st_best);
        for (int i = 1; temp_cur > t_min_; ++i) {
            Range range_cur = shrinkRange(temp_cur);
            State st_next = neighbourState(st_cur, range_cur);
            double en_next = energy_fun_.apply(st_next);
            double en_delta = en_next - energy_fun_.apply(st_cur);
            if (makeTransition(en_delta, temp_cur)) {
                if (en_next < en_best) {
                    en_best = en_next;
                    st_best = st_next;
                }
                st_cur = st_next;
            }
            temp_cur = coolSystem(i);
        }
        return st_best;
    }

private:
    // find state in st_bounds, close to the local minimum
    State neighbourState(const State& st_prev, const Range& st_range) const {
        Range subrange_length = st_range / kSubrangeQuantity;
        State st_pos = st_prev + subrange_length;
        State st_neg = st_prev - subrange_length;
        double state_min  = st_pos;
        double energy_min = energy_fun_.apply(state_min);
        for (int subrange_number = 0; subrange_number < kSubrangeQuantity; ++subrange_number) {
            if (st_pos <= energy_fun_.bounds().second) {
                double energy_st_pos = energy_fun_.apply(st_pos);
                if (energy_st_pos < energy_min) {
                    energy_min = energy_st_pos;
                    state_min = st_pos;
                }
            }
            if (st_neg >= energy_fun_.bounds().first) {
                double energy_st_neg = energy_fun_.apply(st_neg);
                if (energy_st_neg < energy_min) {
                    energy_min = energy_st_neg;
                    state_min = st_neg;
                }
            }
            st_pos += subrange_length;
            st_neg -= subrange_length;
        }
        return state_min;
    }

    // generates new range for states
    Range shrinkRange(Temperature t) const {
        double total_range = energy_fun_.bounds().second - energy_fun_.bounds().first;
        return 0.5 * std::min(total_range, t * total_range);
    }

    Temperature coolSystem(int iter_n) const {
        return t_max_ * std::exp(-iter_n);
    }

    bool makeTransition(double delta_en, double temp) const {
        double rand_num = (double) rand() / RAND_MAX;
        return rand_num < probabilityTransition(delta_en, temp);
    }

    double probabilityTransition(double delta_en, double temp) const {
        return std::exp(-delta_en / temp);
    }

    const GoalFunction<Function>& energy_fun_;
    const int kSubrangeQuantity = 1000;
    Temperature t_min_;
    Temperature t_max_;
};
