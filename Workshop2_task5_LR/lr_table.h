#ifndef LR_TABLE_H
#define LR_TABLE_H

#include <vector>

#include "grammar.h"

class LRTable {
public:
	enum LRTableActionType {
		SHIFT, ACCEPT, GOTO, REDUCE, ERROR
	};
	struct LRTableAction {
		LRTableAction(LRTableActionType _type, int _arg);
		explicit LRTableAction(LRTableActionType _type);
		LRTableActionType type;
		int arg;
	};
	/*struct LRTableState {
		LRTableState();
		int lookahead;
		int
	};*/

	// принимает грамматику, строит таблицу
	LRTable(const Grammar& _grammar);
private:
	Grammar grammar;
	std::vector<std::vector<LRTableAction>> table;
};

#endif // LR_TABLE_H
