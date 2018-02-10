#ifndef AUTOMATON_H
#define AUTOMATON_H
#include <string>
#include <stack>
#include <map>
#include <vector>
#include <memory> // for shared_ptr
#include <cctype> // for isalpha()

/**
 * Class Automaton represents a DFA.
 */
class Automaton {
private:
	static const char EPSILON = '#'; // empty symbol

	/**
	 * State of the DFA. Contains default constructor, method to add a new transition, multimap of transitions
	 * (multimap was chosen to cut down the amount of used memory) and flag (shows if the state has already been processed).
	 */
	struct AutomatonState {
		AutomatonState();
		void addTransition(char letter, std::shared_ptr<AutomatonState> statePtr);
		std::multimap<char, std::shared_ptr<AutomatonState> > transition;
		bool isProcessed;
	};
	typedef std::shared_ptr<AutomatonState> StatePtr;
	typedef std::multimap<char, StatePtr>::iterator MapIterator;

	/**
	 * 3 fields: pointer to the start state, pointer to the final state and the number of states in the DFA.
	 */
	StatePtr startStatePtr;
	StatePtr finalStatePtr;
	size_t stateQuantity;

	void moveByLetter (std::vector<StatePtr>& unprocessedStates, std::vector<StatePtr>& processedStates, char letter) const;
	void epsilonClosure(std::vector<StatePtr>& states) const;
	void removeStateProcessedFlag(const std::vector<StatePtr>& states) const;
public:
	Automaton();
	Automaton(char edgeLetter);
	Automaton(const std::string& regexp);
	Automaton(const Automaton& copyAutomaton); // copy constructor
	Automaton& operator= (const Automaton& oldAutomaton);
	size_t size() const;

	int maxRecognizedWordPrefixLength(const std::string& word) const;
	int maxRecognizedWordSubstrLength(const std::string& word) const;

	friend Automaton plusAutomaton  (const Automaton& left, const Automaton& right);
	friend Automaton crossAutomaton (const Automaton& left, const Automaton& right);
	friend Automaton applyKleeneStar(const Automaton& automaton);
};

Automaton plusAutomaton  (const Automaton& left, const Automaton& right);
Automaton crossAutomaton (const Automaton& left, const Automaton& right);
Automaton applyKleeneStar(const Automaton& automaton);

#endif // AUTOMATON_H
