#include "lr_parser.h"

LRParser::LRParser(LRTable table)
	: table_(table) {}

bool LRParser::checkWord(std::string word) const {
	std::stack<int> parser_stack(1, 0);
	word += "$";
	for (size_t char_index; char_index < word.size(); ++char_index) {
		int current_state = parser_stack.top();
		int current_char_hash =  table_.getGrammar().getTokenHash(word[char_index]);
		switch (table_[current_state][current_char_hash].type) {
			case LRTable::LRTableActionType::ACCEPT: {
				return true;
			}
			case LRTable::LRTableActionType::ERROR: {
				return false;
			}
			case LRTable::LRTableActionType::SHIFT: {
				parser_stack.push(current_state);
				parser_stack.push(table_[current_state][current_char_hash].arg);
				++char_index;
				break;
			}
			case LRTable::LRTableActionType::REDUCE: {
				int rule_number = table_[current_state][current_char_hash].arg;
				for (int pop_index = 0; pop_index < table_.getGrammar().rules_.size(); ++pop_index) {
					parser_stack.pop();
				}
				current_state = parser_stack.top();
				parser_stack.push(table_.getRuleNumber());
				parser_stack.push(table_[current_state][current_char_hash].arg);
				++char_index;
				break;
			}
		}
	}
}
