#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>
#include <string>
#include <iostream>
using std::vector;

template <typename A> using State         = const A;
                      using Position      = const vector<int>;
template <typename A> using Cell          = std::pair<State<A>, Position>;
template <typename A> using Rule          = const State<A> (*const)(const State<A>&, const vector<Cell<A>>&);
template <typename A> using World         = const vector<Cell<A>>;
template <typename A> using Neighbourhood = const vector<Cell<A>> (*const)(World<A>&, Position&);


template <typename A>
Cell<A> apply_rule (World<A>& world, Rule<A> rule, Neighbourhood<A> neighbourhood, const Cell<A> cell) {
    const State<A>& state = cell.first;       // C++14
    Position& position = cell.second;         // C++14
    //const auto& [ state, position ] = cell; // C++17, structured binding
    auto neighbours = neighbourhood(world, position);
    auto newState = rule(state, neighbours);
    return { newState, position };
} 

template <typename A>
World<A> step_time (World<A>& world, Rule<A> rule, Neighbourhood<A> neighbourhood) {
    vector<Cell<A>> result;
    std::transform(world.cbegin(), world.cend(), std::back_inserter(result), 
        [&world, rule, neighbourhood](const auto& cell) { return apply_rule(world, rule, neighbourhood, cell); } );
    return result;
}

//-----------------

enum class LifeStates : bool {
    Alive = true, Dead = false
};

const State<LifeStates> rule_life(const State<LifeStates>& state, const vector<Cell<LifeStates>>& neighbours) {
    auto is_alive = [](const auto& cell) { return (bool)cell.first; };
    auto count = count_if(neighbours.cbegin(), neighbours.cend(), is_alive);
    switch(state){
        case LifeStates::Alive: return (count < 2 || count > 3) ? LifeStates::Dead : LifeStates::Alive;
        case LifeStates::Dead: return count == 3 ? LifeStates::Alive : LifeStates::Dead;
    }
}

const vector<Cell<LifeStates>> neighbourhood_life(World<LifeStates>& world, Position& pos) {
    auto is_near = [](const int c1, const int c2) { return abs(c1-c2) <= 1; };
    vector<Cell<LifeStates>> result;
    std::copy_if(world.begin(), world.end(), std::back_inserter(result), [&pos, is_near](const auto& cell){
        Position& p = cell.second;
        return std::inner_product(pos.cbegin(), pos.cend(), p.cbegin(), true, std::logical_and<>(), is_near)
           && p != pos;
    });
    return result;
}

//-----------------

World<LifeStates> generate_world(const std::string& str) {
    auto generate_state = [](char ch){ return ch == '#' ? LifeStates::Alive : LifeStates::Dead; };
    vector<Cell<LifeStates>> result;
    int i = 0, j = 0;
    for (char c: str) {
        if (c == '\n') { i++; j = 0; continue; }
        result.push_back({ generate_state(c), { i, j++ } });
    }
    return result;
}

auto initial_state = generate_world(R"(
0#00000000
00#0000000
###0000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000)");

void print_world(World<LifeStates>& world) {
    auto print_cell = [](const State<LifeStates>& state) { return ((bool)state) ? "#" : "0"; };
    auto len = std::max_element(world.cbegin(), world.cend(), [](const auto& a, const auto& b){
        return a.second.back() < b.second.back();
    })->second.back();
    auto res = std::accumulate(world.cbegin(), world.cend(), std::string(), [len, print_cell](const auto& acc, const auto& cell){
        const State<LifeStates>& s = cell.first;  // C++14
        Position& p = cell.second;                // C++14
        // const auto& [ s, p ] = cell;           // C++17, structured binding
        return acc + print_cell(s) + (p.back() == len ? "\n" : "");
    });
    std::cout << res;
}

int main() {
    print_world(step_time(initial_state, rule_life, neighbourhood_life));
    return 0;
}