#ifndef GRAMMAR_H
#define GRAMMAR_H

#include <fstream>
#include <functional>
#include <string>
#include <unordered_set>
#include <vector>

enum ProductionTokenType {
	ProductionTokenTypeTerminal, ProductionTokenTypeNonterminal, ProductionTokenTypeEnd
};

class ProductionToken {
public:
	ProductionToken(const ProductionTokenType& type, const std::string& name);
	bool operator== (const ProductionToken& other) const;
	const std::string& getName() const;
	const ProductionTokenType& getType() const;
private:
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

struct ProductionRuleHasher {
	size_t operator() (const ProductionRule& rule) const;
};

class Grammar {
public:
	Grammar();
private:
	std::unordered_multiset<ProductionRule, ProductionRuleHasher> rules;
};

Grammar readGrammar(std::ifstream& fin);

#endif // LRGRAMMAR_H
