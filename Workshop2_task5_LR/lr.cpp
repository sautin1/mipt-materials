#include <bits/stdc++.h>
#include "lr.h"

using namespace std;

void Symbol::initialize() {
    string accepting = "qwertyuiopasdfghjklzxcvbnm()+-*";
    for (char ch : accepting)
        terminalChars.insert(ch);

    add("");
    add("$");

    epsId = nameToId[""];
    endId = nameToId["$"];
}

void Symbol::addDot() {
    add(".");
    dotId = nameToId["."];
}

bool Symbol::isTerminal(const string &name) {
    if (name == "" || name == "$")
        return true;
    return name.length() == 1 && terminalChars.count(name[0]) == 1;
}

bool Symbol::isTerminal(int id) {
    return isTerminal(idToName[id]);
}

bool Symbol::isSpecial(int id) {
    string w = idToName[id];
    return w == "" || w == "$" || w == ".";
}

void Symbol::add(const string &name) {
    if (nameToId.count(name) == 0) {
        nameToId[name] = lastUnusedId;
        idToName[lastUnusedId] = name;
        ++lastUnusedId;
    }
}

Rule::Rule(const string &s) {
    stringstream stream;
    string temp;

    stream << s;

    stream >> temp;
    Symbol::add(temp);
    from = Symbol::nameToId[temp];

    while (stream >> temp) {
        Symbol::add(temp);
        to.push_back(Symbol::nameToId[temp]);
    }

    if (to.empty()) {
        to.push_back(Symbol::nameToId[""]);
    }
}

void Grammar::extend() {
    Symbol::add("@");
    int st = Symbol::nameToId["@"];
    rules.emplace_back(st, vector<int>(1, start));
    start = st;
}

bool Grammar::addToFirst(int id, int x) {
    if (id == Symbol::endId || x == Symbol::endId)
        return false;
    if (first[id].count(x) == 0) {
        first[id].insert(x);
        return true;
    }
    return false;
}

void Grammar::calcFirst() {
    bool changed = true;
    while (changed) {
        changed = false;
        for (int id = 0; id < Symbol::lastUnusedId; ++id)
            if (Symbol::isTerminal(id)) {
                changed |= addToFirst(id, id);
            }

        for (auto &r : rules) {
            if (r.to.size() == 1 && r.to[0] == Symbol::epsId) {
                changed |= addToFirst(r.from, Symbol::epsId);
            }
            else {
                bool containEps = true;
                for (int i = 0; i < (int) r.to.size(); ++i) {
                    int id = r.to[i];

                    for (int x : first[id])
                        changed |= addToFirst(r.from, x);

                    if (first[id].count(Symbol::epsId) == 0) {
                        containEps = false;
                        break;
                    }
                }

                if (containEps)
                    changed |= addToFirst(r.from, Symbol::epsId);
            }
        }
    }
}

set<int> Grammar::getFirst(int id) {
    return first[id];
}

set<int> Grammar::getFirst(const vector<int> &word) {
    set<int> ans;
    bool containEps = true;
    for (int i = 0; containEps && i < (int) word.size(); ++i) {
        int c = word[i];
        for (int x : getFirst(c))
            if (x != Symbol::epsId)
                ans.insert(x);
        containEps &= getFirst(c).count(Symbol::epsId);
    }
    if (containEps)
        ans.insert(Symbol::epsId);
    return ans;
}

bool Grammar::addToFollow(int id, int x) {
    if (id == Symbol::epsId || x == Symbol::epsId)
        return false;
    if (follow[id].count(x) == 0) {
        follow[id].insert(x);
        return true;
    }
    return false;
}

void Grammar::calcFollow() {
    bool changed = true;
    while (changed) {
        changed = false;
        changed |= addToFollow(start, Symbol::endId);

        for (auto &r : rules) {
            for (int i = 0; i + 1 < (int) r.to.size(); ++i) {
                int id = r.to[i];
                if (Symbol::isTerminal(id))
                    continue;
                for (auto &x : getFirst(r.to[i + 1]))
                    if (x != Symbol::epsId)
                        changed |= addToFollow(id, x);
            }

            for (int i = 0; i < (int) r.to.size(); ++i) {
                int id = r.to[i];
                if (Symbol::isTerminal(id))
                    continue;
                if (i + 1 == (int) r.to.size() || getFirst(r.to[i + 1]).count(Symbol::epsId) == 1)
                    for (int x : follow[r.from])
                        changed |= addToFollow(id, x);
            }
        }
    }
}

void Grammar::eliminateNullable() {
    vector<bool> isNullable(Symbol::lastUnusedId, false);
    isNullable[Symbol::epsId] = true;
    for (int i = 0; i <= (int) rules.size(); ++i) {
        for (int j = 0; j < (int) rules.size(); ++j)
            if (!isNullable[rules[j].from]) {
                bool nullable = true;
                for (int &to : rules[j].to)
                    nullable &= isNullable[to];
                isNullable[rules[j].from] = nullable;
            }
    }

    vector<Rule> newRules;
    for (Rule &r : rules) {
        vector<vector<int>> to(1);
        for (int &s : r.to)
            if (!isNullable[s]) {
                for (auto &x : to)
                    x.push_back(s);
            }
            else {
                if (s == Symbol::epsId)
                    continue; 
                int n = to.size();
                for (int i = 0; i < n; ++i) {
                    to.push_back(to[i]);
                    to[i].push_back(s);
                }
            }
        for (auto &x : to)
            if (x.size() != 0) {
                newRules.emplace_back(r.from, x);
            }
    }

    rules = newRules;
}

Grammar readGrammar(ifstream &input) {
    Grammar ans;
    int n;
    string temp;

    input >> n;
    getline(input, temp, '\n');
    for (int i = 0; i < n; ++i) {
        getline(input, temp, '\n');
        ans.rules.emplace_back(temp);
    }

    ans.start = ans.rules.front().from;

    return ans;
}

int State::getDotPosition() {
    for (int i = 0; i < (int) to.size(); ++i)
        if (to[i] == Symbol::dotId)
            return i;
    cerr << "Invalid state has been given to getDotPosition. Aborting...\n";
    exit(-1);
}

void State::normalize() {
    if (lookahead == Symbol::epsId)
        lookahead = Symbol::endId;
}

bool State::operator < (const State &T) const {
    auto p = make_pair(from, make_pair(to, lookahead));
    auto p1 = make_pair(T.from, make_pair(T.to, T.lookahead));
    return p < p1;
}

bool State::operator == (const State &T) const {
    auto p = make_pair(from, make_pair(to, lookahead));
    auto p1 = make_pair(T.from, make_pair(T.to, T.lookahead));
    return p == p1;
}




Set SetConstruction::closure(Set s, Grammar &g) {
    while (true) {
        vector<State> toAdd;
        for (auto st : s) {
            int dotPos = st.getDotPosition();
            if (dotPos == (int) st.to.size() - 1)
                continue;
            int B = st.to[dotPos + 1];
            vector<int> suffix(st.to.begin() + dotPos + 2, st.to.end());
            if (st.lookahead != Symbol::endId)
                suffix.push_back(st.lookahead);
            for (auto r : g.rules)
                if (r.from == B)
                    for (auto b : g.getFirst(suffix))
                        toAdd.emplace_back(B, r.to, 0, b);
        }

        bool changed = false;
        for (auto x : toAdd)
            if (s.count(x) == 0) {
                changed = true;
                s.insert(x);
            }

        if (!changed)
            return s;
    }
}

Set SetConstruction::goTo(Set I, int x, Grammar &g) {
    Set J;
    for (State S : I) {
        int dotPos = S.getDotPosition();
        
        if (dotPos == (int) S.to.size() - 1 || S.to[dotPos + 1] != x)
            continue;
        swap(S.to[dotPos], S.to[dotPos + 1]);
        J.insert(S);
    }
    J = closure(J, g);
    return closure(J, g);
}

vector<Set> SetConstruction::items(Grammar &g) {
    vector<Set> C;
    State state(g.start, g.rules.back().to, 0, Symbol::endId);
    Set st;
    st.insert(state);
    C.push_back(closure(st, g));

    bool changed = true;

    while (changed) {
        changed = false;
        for (Set &I : C) {
            vector<Set> toAdd;
            for (int id = 0; id < Symbol::lastUnusedId; ++id) {
                if (Symbol::isSpecial(id))
                    continue;

                auto J = goTo(I, id, g);
                if (J.size() != 0)
                    toAdd.push_back(J);
            }

            for (auto J : toAdd)
                if (find(C.begin(), C.end(), J) == C.end()) {
                    C.push_back(J);
                    changed = true;
                }
        }
    }

    return C;
}  

bool LRAnalyzer::buildTable(Grammar &g) {
    item = SetConstruction::items(g);

    for (int i = 0; i < (int) item.size(); ++i) {
        for (auto st : item[i]) {
            int dotPos = st.getDotPosition(), a;
            if (dotPos == (int) st.to.size() - 1) {
                vector<int> to(st.to.begin(), st.to.end() - 1);
                int a = st.lookahead;
                Rule r(st.from, to);

                if (Symbol::idToName[st.from] == "@" && st.lookahead == Symbol::endId) {

                    action[make_pair(i, a)] = Action(ACCEPT);
                } else {
                    if (action[make_pair(i, a)].type != ERROR)
                        return false;

                    action[make_pair(i, a)] = Action(r);
                }
            } else {

                a = st.to[dotPos + 1];
                if (!Symbol::isTerminal(a))
                    continue;
                auto toItem = SetConstruction::goTo(item[i], a, g);
                if (toItem.size() == 0)
                    continue;

                int j = find(item.begin(), item.end(), toItem) - item.begin();

                action[make_pair(i, a)] = Action(j);
            }
        }
    }

    for (int i = 0; i < (int) item.size(); ++i)
        for (int id = 0; id < Symbol::lastUnusedId; ++id)
            if (!Symbol::isTerminal(id)) {
                auto J = SetConstruction::goTo(item[i], id, g);
                if (J.empty())
                    continue;
                int j = find(item.begin(), item.end(), J) - item.begin();
                goTo[make_pair(i, id)] = j;
            }
    return true;
}

bool LRAnalyzer::routine(string word) {
    word = word + "$";
    vector<int> w;
    int ip = 0;
    stack<int> st;

    for (int i = 0; i < (int) word.length(); ++i)
        w.push_back(Symbol::nameToId[word.substr(i, 1)]);

    st.push(0);

    while (true) {
        int s = st.top();
        
        int a = w[ip];
        Action act = action[make_pair(s, a)];

        if (act.type == SHIFT) {
            st.push(a);
            st.push(act.shift);
            ++ip;
        } else if (act.type == REDUCE) {
            for (int i = 0; i < 2 * (int) act.reduce.to.size(); ++i)
                st.pop();
            s = st.top();
            a = act.reduce.from;
            st.push(a);
            st.push(goTo[make_pair(s, a)]);
        } else if (act.type == ACCEPT) {
            return true;
        } else {
            return false;
        }
    }
}


int main(int argv, char **argc) {
    if (argv != 2) {
        cerr << "Wrong number of input files. Should be exactly one.\n";
        return -1;
    }

    Symbol::initialize();
    ifstream input(argc[1]);
    if (!input.good()) {
    	throw new std::runtime_error("wrong filename");
    }
    Grammar g = readGrammar(input);
    g.eliminateNullable();
    g.extend();

    g.calcFirst();

    g.calcFollow();

    Symbol::addDot();

    LRAnalyzer a;
    if (!a.buildTable(g)) {
        cerr << "Input grammar is not LR-grammar. Aborting...\n";
        return -1;
    }

    int m;
    cin >> m;
    while (m--) {
        string w;
        cin >> w;
        cout << (a.routine(w) ? "YES\n" : "NO\n");
    }

    return 0;
}
