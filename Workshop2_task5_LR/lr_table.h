#ifndef LR_TABLE_H
#define LR_TABLE_H

class LRTable {
public:
	enum LRTableAction {
		SHIFT, ACCEPT, GOTO, REDUCE, ERROR
	};


	// принимает грамматику, строит таблицу
	LRTable();
};

#endif // LR_TABLE_H
