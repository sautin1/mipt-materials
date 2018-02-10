#include "automaton.h"

Automaton::AutomatonState::AutomatonState()
	: isProcessed(false) {}


void Automaton::AutomatonState::addTransition(char letter, std::shared_ptr<AutomatonState> statePtr) {
	transition.insert(std::pair<char, StatePtr>(letter, statePtr));
}

Automaton::Automaton() {
	startStatePtr = std::make_shared<AutomatonState>();
	finalStatePtr = std::make_shared<AutomatonState>();
	startStatePtr->addTransition(EPSILON, finalStatePtr);
	stateQuantity = 2;
}

Automaton::Automaton(char edgeLetter) {
	startStatePtr = std::make_shared<AutomatonState>();
	finalStatePtr = std::make_shared<AutomatonState>();
	startStatePtr->addTransition(edgeLetter, finalStatePtr);
	stateQuantity = 2;
}

Automaton::Automaton(const Automaton& copyAutomaton) {
	startStatePtr = copyAutomaton.startStatePtr;
	finalStatePtr = copyAutomaton.finalStatePtr;
	stateQuantity = copyAutomaton.stateQuantity;
}

Automaton& Automaton::operator= (const Automaton& copyAutomaton) {
	startStatePtr = copyAutomaton.startStatePtr;
	finalStatePtr = copyAutomaton.finalStatePtr;
	stateQuantity = copyAutomaton.stateQuantity;
	return *this;
}

size_t Automaton::size() const {
	return stateQuantity;
}

Automaton::Automaton(const std::string& regexp) {
	std::stack<Automaton> automatonStack;
	for (size_t charIndex = 0; charIndex < regexp.size(); ++charIndex) {
		switch (regexp[charIndex]) {
			case '+': {
				if (automatonStack.size() < 2) {
					throw std::logic_error("operation before operand at position " + charIndex);
				}
				Automaton leftAutomaton = automatonStack.top();
				automatonStack.pop();
				Automaton rightAutomaton = automatonStack.top();
				automatonStack.pop();
				automatonStack.push(plusAutomaton(leftAutomaton, rightAutomaton));
				break;
			}
			case '.': {
				if (automatonStack.size() < 2) {
					throw std::logic_error("operation before operand at position " + charIndex);
				}
				Automaton rightAutomaton = automatonStack.top();
				automatonStack.pop();
				Automaton leftAutomaton = automatonStack.top();
				automatonStack.pop();
				automatonStack.push(crossAutomaton(leftAutomaton, rightAutomaton));
				break;
			}
			case '*': {
				Automaton topAutomaton = automatonStack.top();
				automatonStack.pop();
				automatonStack.push(applyKleeneStar(topAutomaton));
				break;
			}
			case '1': {
				automatonStack.push(Automaton());
				break;
			}
			default: {
				if (!isalpha(regexp[charIndex])) {
					throw std::logic_error("not an alphabet symbol at position " + charIndex);
				}
				automatonStack.push(Automaton(regexp[charIndex]));
			}
		}
	}
	if (automatonStack.size() != 1) {
		throw std::logic_error("wrong number of operands");
	}
	*this = automatonStack.top();
}

/**
 * Generates a new set of states in DFA and stores them in <processedStates>.
 * A new set contains those states that can be reached from any of the states in <unprocessedStates> by single <letter>-transition.
 */
void Automaton::moveByLetter(std::vector<StatePtr>& unprocessedStates, std::vector<StatePtr>& processedStates, char letter) const {
	for (size_t stateIndex = 0; stateIndex < unprocessedStates.size(); ++stateIndex) {
		std::pair<MapIterator, MapIterator> itPair = unprocessedStates[stateIndex]->transition.equal_range(letter);
		for (MapIterator it = itPair.first; it != itPair.second; ++it) {
			if (!it->second->isProcessed) {
				processedStates.push_back(it->second);
				it->second->isProcessed = true;
			}
		}
	}
}

/**
 * Extends the set of states stored in <states> vector.
 * All the states that can be reached from those in <states> by any number of epsilon-transitions are added to <states>.
 */
void Automaton::epsilonClosure(std::vector<StatePtr>& states) const {
	moveByLetter(states, states, EPSILON);
}

/**
 * Sets flag <isProcessed> false for all the states in <state> vector.
 */
void Automaton::removeStateProcessedFlag(const std::vector<StatePtr>& states) const {
	for (size_t stateIndex = 0; stateIndex < states.size(); ++stateIndex) {
		states[stateIndex]->isProcessed = false;
	}
}

/**
 * Returns the length of the longest prefix of the <word> which is recognized (accepted) by DFA.
 */
int Automaton::maxRecognizedWordPrefixLength(const std::string& word) const {
	std::vector<StatePtr> processedStates;
	std::vector<StatePtr> unprocessedStates(1, startStatePtr);
	startStatePtr->isProcessed = true;
	epsilonClosure(unprocessedStates);

	int maxRecognizedPrefixLength = -1;
	if (finalStatePtr->isProcessed) {
		maxRecognizedPrefixLength = 0;
	}
	removeStateProcessedFlag(unprocessedStates);
	for (size_t letterIndex = 0; letterIndex < word.size(); ++letterIndex) {
		moveByLetter(unprocessedStates, processedStates, word[letterIndex]);
		std::swap(unprocessedStates, processedStates);
		processedStates.clear();
		epsilonClosure(unprocessedStates);
		if (finalStatePtr->isProcessed) {
			maxRecognizedPrefixLength = letterIndex + 1;
		}
		removeStateProcessedFlag(unprocessedStates);
		if (unprocessedStates.empty()) {
			break;
		}
	}
	return maxRecognizedPrefixLength;
}

/**
 * Returns the length of the longest substring of the <word> which is recognized (accepted) by DFA.
 */
int Automaton::maxRecognizedWordSubstrLength(const std::string& word) const {
	int maxRecognizedSubstrLength = -1;
	for (int suffixStartIndex = 0; suffixStartIndex < (int)word.length(); ++suffixStartIndex) {
		std::string suffix = word.substr(suffixStartIndex, word.length());
		int currentRecognizedSubstrLength = maxRecognizedWordPrefixLength(suffix);
		maxRecognizedSubstrLength = std::max(maxRecognizedSubstrLength, currentRecognizedSubstrLength);
		if (maxRecognizedSubstrLength == (int)word.length()) {
			break;
		}
	}
	return maxRecognizedSubstrLength;
}

/**
 * Returns the automaton which represents the union of <left> and <right>.
 * If <left> was constructed by regexp <a> and <right> - by regexp <b>, then the automaton returned is constructed by <a+b>.
 */
Automaton plusAutomaton(const Automaton& left, const Automaton& right) {
	Automaton automaton;
	automaton.startStatePtr = std::make_shared<Automaton::AutomatonState>();
	automaton.finalStatePtr = std::make_shared<Automaton::AutomatonState>();
	automaton.startStatePtr->addTransition(Automaton::EPSILON, left.startStatePtr);
	automaton.startStatePtr->addTransition(Automaton::EPSILON, right.startStatePtr);
	left.finalStatePtr->addTransition(Automaton::EPSILON, automaton.finalStatePtr);
	right.finalStatePtr->addTransition(Automaton::EPSILON, automaton.finalStatePtr);
	automaton.stateQuantity = left.stateQuantity + right.stateQuantity + 2;
	return automaton;
}

/**
 * Returns the automaton which represents the intersection of <left> and <right>.
 * If <left> was constructed by regexp <a> and <right> - by regexp <b>, then the automaton returned is constructed by <a.b>.
 */
Automaton crossAutomaton(const Automaton& left, const Automaton& right) {
	Automaton automaton;
	automaton.startStatePtr = left.startStatePtr;
	automaton.finalStatePtr = right.finalStatePtr;
	left.finalStatePtr->addTransition(Automaton::EPSILON, right.startStatePtr);
	automaton.stateQuantity = left.stateQuantity + right.stateQuantity;
	return automaton;
}

/**
 * Returns the automaton which represents the <automaton> with the Kleene star applied.
 * If <automaton> was constructed by regexp <a>, then the automaton returned is constructed by <a*>.
 */
Automaton applyKleeneStar(const Automaton& automaton) {
	Automaton newAutomaton(automaton);
	Automaton::StatePtr newStartState = std::make_shared<Automaton::AutomatonState>();
	Automaton::StatePtr newFinalState = std::make_shared<Automaton::AutomatonState>();
	newStartState->addTransition(Automaton::EPSILON, newFinalState);
	newStartState->addTransition(Automaton::EPSILON, newAutomaton.startStatePtr);
	newAutomaton.finalStatePtr->addTransition(Automaton::EPSILON, newAutomaton.startStatePtr);
	newAutomaton.finalStatePtr->addTransition(Automaton::EPSILON, newFinalState);
	newAutomaton.startStatePtr = newStartState;
	newAutomaton.finalStatePtr = newFinalState;
	newAutomaton.stateQuantity += 2;
	return newAutomaton;
}
