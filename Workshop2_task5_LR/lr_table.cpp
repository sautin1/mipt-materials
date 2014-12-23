#include "lr_table.h"

LRTable::LRTableAction::LRTableAction(LRTableActionType _type, int _arg)
	: type(_type), arg(_arg) {}

LRTable::LRTableAction::LRTableAction(LRTableActionType _type)
	: type(_type), arg(0) {}

LRTable::LRTable(const Grammar& _grammar)
	: grammar_(_grammar) {}

Grammar& LRTable::getGrammar() const {
	return grammar_;
}

std::vector<LRTableAction>& LRTable::operator[] (int index) const {
	return table_[index];
}
