#include "grammar.h"

// ProductionToken

ProductionToken::ProductionToken()
	: nonterminal_alphabet(nonterminal_name_letters.begin(), nonterminal_name_letters.end()),
	  terminal_alphabet(terminal_name_letters.begin(), terminal_name_letters.end()),
	  end_letters_alphabet(end_name_letters.begin(), end_name_letters.end()) {}

ProductionToken::ProductionToken(const std::string& name)
	: nonterminal_alphabet(nonterminal_name_letters.begin(), nonterminal_name_letters.end()),
	  terminal_alphabet(terminal_name_letters.begin(), terminal_name_letters.end()),
	  end_letters_alphabet(end_name_letters.begin(), end_name_letters.end()), name_(name) {
	if (terminal_alphabet.count(name_[0]) == 1) {
		type_ = ProductionTokenTypeTerminal;
	} else if (nonterminal_alphabet.count(name_[0]) == 1) {
		type_ = ProductionTokenTypeNonterminal;
	} else if (end_letters_alphabet.count(name_[0]) == 1) {
		type_ = ProductionTokenTypeEnd;
	} else {
		throw std::logic_error("Wrong production token name");
	}
}

ProductionToken::ProductionToken(const ProductionTokenType& type, const std::string& name)
	: nonterminal_alphabet(nonterminal_name_letters.begin(), nonterminal_name_letters.end()),
	  terminal_alphabet(terminal_name_letters.begin(), terminal_name_letters.end()),
	  end_letters_alphabet(end_name_letters.begin(), end_name_letters.end()), type_(type), name_(name) {}

bool ProductionToken::operator== (const ProductionToken& other) const {
	return name_ == other.name_;
}

const std::string& ProductionToken::getName() const {
	return name_;
}

const ProductionTokenType& ProductionToken::getType() const {
	return type_;
}

// ProductionRule

ProductionRule::ProductionRule(const ProductionToken& left, const std::vector<ProductionToken>& right)
	:left_(left), right_(right) {}

bool ProductionRule::operator== (const ProductionRule& other) const {
	return left_ == other.left_;
}

const ProductionToken& ProductionRule::getLeft() const {
	return left_;
}

const std::vector<ProductionToken>& ProductionRule::getRight() const {
	return right_;
}

// ProductionTokenHasher

size_t ProductionTokenHasher::operator() (const ProductionToken& token) const {
	return std::hash<std::string>()(token.getName());
}

// Grammar

int Grammar::getTokenHash(const ProductionToken& token) {
	auto token_it = token_hashes_.find(token);
	if (token_it != token_hashes_.end()) {
		return token_it->second;
	} else {
		return token_hashes_[token] = ++max_hash;
	}
}

void Grammar::addRule(const ProductionRule& rule) {
	int left_hash = getTokenHash(rule.getLeft());
	std::vector<int> right_hashes;
	for (size_t token_index = 0; token_index < rule.getRight().size(); ++token_index) {
		right_hashes.push_back(getTokenHash(rule.getRight().at(token_index)));
	}
	rules_.insert(std::pair<int, std::vector<int>>(left_hash, right_hashes));
}

// Misc

Grammar readGrammar(std::ifstream& fin) {
	Grammar grammar;
	int size;
	fin >> size;
	for (int rule_index = 0; rule_index < size; ++rule_index) {
		std::string rule_string;
		std::getline(fin, rule_string);
		std::istringstream sstream(rule_string);
		std::string rule_token;
		sstream >> rule_token;
		ProductionToken left_token(ProductionTokenTypeNonterminal, rule_token);
		std::vector<ProductionToken> right_tokens;
		while (sstream >> rule_token) {
			right_tokens.push_back(ProductionToken(rule_token));
		}
		grammar.addRule(ProductionRule(left_token, right_tokens));
	}
	return grammar;
}
