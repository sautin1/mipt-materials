#include <bits/stdc++.h>

using namespace std;

namespace Symbol {
    int lastUnusedId = 0;
    int epsId, endId, dotId;
    map<string, int> nameToId;
    map<int, string> idToName;
    set<char> terminalChars;
    void initialize();
    void addDot();
    bool isTerminal(const string &name);
    bool isTerminal(int id);
    bool isSpecial(int id);
    void add(const string &name);
}

struct Rule {
    int from;
    vector<int> to;

    Rule() {}
    Rule(int from, vector<int> to) : from(from), to(to) {}
    Rule(const string &s);
};

struct Grammar {
    int start;
    vector<Rule> rules;
    map<int, set<int>> first;
    map<int, set<int>> follow;

    Grammar(){}
    Grammar(int start, vector<Rule> rules) : start(start), rules(rules) {}

    void extend();
    void eliminateNullable();
    
    bool addToFirst(int id, int x);
    void calcFirst();
    set<int> getFirst(int id);
    set<int> getFirst(const vector<int> &word);

    bool addToFollow(int id, int x);
    void calcFollow();

    void write();
    void writeFirst();
    void writeFollow();
};

struct State {
    int from;
    vector<int> to;
    int lookahead;

    void normalize();
    State() {}
    State(int from, vector<int> to, int lookahead) : from(from), to(to), lookahead(lookahead) {
        normalize();
    }
    State(int from, vector<int> to_, int dotPos, int lookahead) : from(from), to(to_), lookahead(lookahead) {
        to.insert(to.begin() + dotPos, Symbol::dotId);
        normalize();
    }


    bool operator < (const State &T) const;

    bool operator == (const State &T) const;

    int getDotPosition();
};

typedef set<State> Set;

namespace SetConstruction {
    Set closure(Set s, Grammar &g);
    Set goTo(Set s, int x, Grammar &g);
    vector<Set> items(Grammar &g);

    void writeConstructedSets(Grammar &g);
}

Grammar readGrammar(ifstream &input);

enum ActionType {
    ACCEPT, REDUCE, SHIFT, ERROR
};

struct Action {
    ActionType type;
    Rule reduce;
    int shift;

    Action() : type(ERROR) {}
    explicit Action(Rule r) : type(REDUCE), reduce(r) {}
    explicit Action(int shift) : type(SHIFT), shift(shift) {}
    explicit Action(ActionType type) : type(type) {}
};

class LRAnalyzer {
private:
    map<pair<int, int>, Action> action;
    map<pair<int, int>, int> goTo;
    vector<Set> item;
public:
    bool buildTable(Grammar &g);
    void writeTable();
    bool routine(string w);
};

namespace Debug {
    void writeState(const State &st);
    string toString(const Action &act);
}