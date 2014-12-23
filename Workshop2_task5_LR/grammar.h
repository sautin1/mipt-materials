#ifndef GRAMMAR_H
#define GRAMMAR_H

#include <fstream>
#include <functional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

enum ProductionTokenType {
	ProductionTokenTypeTerminal, ProductionTokenTypeNonterminal, ProductionTokenTypeEnd
};

class ProductionToken {
public:
	ProductionToken();
	explicit ProductionToken(const std::string& name);
	ProductionToken(const ProductionTokenType& type, const std::string& name);
	bool operator== (const ProductionToken& other) const;
	const std::string& getName() const;
	const ProductionTokenType& getType() const;
private:
	typedef std::unordered_set<char> Alphabet;
	const std::string nonterminal_name_letters = "abcdefghijklmnopqrstuvwxyz";
	const Alphabet nonterminal_alphabet;
	const std::string terminal_name_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	const Alphabet terminal_alphabet;
	const std::string end_name_letters = "$";
	const Alphabet end_letters_alphabet;
	ProductionTokenType type_;
	std::string name_;
};

struct ProductionRule {
public:
	ProductionRule(const ProductionToken& left, const std::vector<ProductionToken>& right);
	bool operator== (const ProductionRule& other) const;
	const ProductionToken& getLeft() const;
	const std::vector<ProductionToken>& getRight() const;
private:
	ProductionToken left_;
	std::vector<ProductionToken> right_;
};

struct ProductionTokenHasher {
	size_t operator() (const ProductionToken& token) const;
};

class Grammar {
public:
	Grammar() = default;
	void addRule(const ProductionRule& rule);
	int getTokenHash(const ProductionToken& token);
private:

	int max_hash;
	std::unordered_map<ProductionToken, int, ProductionTokenHasher> token_hashes_;
	std::unordered_multimap<int, std::vector<int>> rules_; // индексируется хешами токена, стоящего слева.
};

Grammar readGrammar(std::ifstream& fin);

#endif // LRGRAMMAR_H
