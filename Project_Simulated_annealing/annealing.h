#pragma once
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <utility>
#include <vector>

using StateMarginal = double;
using State = std::vector<StateMarginal>;   // point in R^n
using TemperatureMarginal = double;
using Temperature = std::vector<TemperatureMarginal>;

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

    Function func() const {
        return func_;
    }
private:
    Function func_;
    std::pair<State, State> bounds_;
};

template <typename Function>
class Annealing {
public:
    using StateBounds = std::pair<State, State>;
    using RangeMarginal = double;
    using Range = std::vector<RangeMarginal>;

    Annealing(const GoalFunction<Function>& energy_f, Temperature t_min, Temperature t_max)
        : energy_fun_(energy_f), t_min_(t_min), t_max_(t_max) {}

    State annealMin(State st_cur) const {
        Temperature temp_cur = t_max_;
        State st_best = st_cur;
        double en_best = energy_fun_.apply(st_best);
        for (int iter = 1; !std::equal(temp_cur.begin(), temp_cur.end(), t_min_.begin(), std::less_equal<double>()); ++iter) {
            Range range_cur = shrinkRange(temp_cur);
            State st_next = neighbourState(st_cur, range_cur);
            double en_next = energy_fun_.apply(st_next);
            double en_delta = en_next - energy_fun_.apply(st_cur);
            if (en_next < en_best) {
                en_best = en_next;
                st_best = st_next;
            }
            for (size_t i = 0; i < st_cur.size(); ++i) {
                if (makeTransition(en_delta, temp_cur[i])) {
                    st_cur[i] = st_next[i];
                }
            }
            temp_cur = coolSystem(iter);
        }
        return st_best;
    }

private:
    const int kSubrangeQuantity = 1000;

    StateMarginal neighbourStateMarginal(State state, RangeMarginal st_range, int arg_n) const {
        RangeMarginal subrange_length = st_range / kSubrangeQuantity;
        StateMarginal st_pos = state[arg_n] + subrange_length;
        StateMarginal st_neg = state[arg_n] - subrange_length;
        StateMarginal state_min  = st_pos;
        state[arg_n] = state_min;
        double energy_min = energy_fun_.apply(state);
        for (int subrange_number = 0; subrange_number < kSubrangeQuantity; ++subrange_number) {
            if (st_pos <= energy_fun_.bounds().second[arg_n]) {
                state[arg_n] = st_pos;
                double energy_st_pos = energy_fun_.apply(state);
                if (energy_st_pos < energy_min) {
                    energy_min = energy_st_pos;
                    state_min = st_pos;
                }
            }
            if (st_neg >= energy_fun_.bounds().first[arg_n]) {
                state[arg_n] = st_neg;
                double energy_st_neg = energy_fun_.apply(state);
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

    // find state in st_bounds, close to the local minimum
    State neighbourState(State st_prev, const Range& st_range) const {
        State st_new;
        st_new.reserve(st_prev.size());
        for (size_t i = 0; i < st_prev.size(); ++i) {
            StateMarginal neighbour = neighbourStateMarginal(st_prev, st_range[i], i);
            st_new.push_back(neighbour);
            st_prev[i] = neighbour;
        }
        return st_new;
    }

    // generates new range for states
    Range shrinkRange(const Temperature& t) const {
        Range new_range;
        new_range.reserve(t.size());
        for (size_t i = 0; i < t.size(); ++i) {
            double total_range = energy_fun_.bounds().second[i] - energy_fun_.bounds().first[i];
            new_range.push_back(total_range * std::min(1.0, t[i]));
        }
        return new_range;
    }

    Temperature coolSystem(int iter_n) const {
        Temperature temp;
        temp.reserve(t_max_.size());
        for (size_t i = 0; i < t_max_.size(); ++i) {
            temp.push_back(t_max_[i] * std::exp(-iter_n));
        }
        return temp;
    }

    bool makeTransition(double delta_en, TemperatureMarginal temp) const {
        double rand_num = (double) rand() / RAND_MAX;
        double prob = probabilityTransition(delta_en, temp);
        return rand_num < prob;
    }

    double probabilityTransition(double delta_en, double temp) const {
        return std::exp(-delta_en / temp);
    }

    const GoalFunction<Function>& energy_fun_;
    Temperature t_min_;
    Temperature t_max_;
};
