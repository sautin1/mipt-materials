#ifndef LR_PARSER_H
#define LR_PARSER_H

#include <stack>
#include <string>

#include "lr_table.h"

class LRParser {
public:
	LRParser(LRTable table);
	bool checkWord(std::string word) const;
private:
	LRTable table_;
};

#endif // LR_PARSER_H
