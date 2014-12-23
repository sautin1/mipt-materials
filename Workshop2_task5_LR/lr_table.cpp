#include "lr_table.h"

LRTable::LRTableAction::LRTableAction(LRTableActionType _type, int _arg)
	: type(_type), arg(_arg) {}

LRTable::LRTableAction::LRTableAction(LRTableActionType _type)
	: type(_type), arg(0) {}

LRTable::LRTable(const Grammar& _grammar)
	: grammar(_grammar) {}
