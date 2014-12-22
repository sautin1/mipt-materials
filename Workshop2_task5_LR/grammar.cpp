#include "grammar.h"

// ProductionToken

ProductionToken::ProductionToken(const ProductionTokenType& type, const std::string& name)
	: type_(type), name_(name) {}

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

// ProductionRuleHasher

size_t ProductionRuleHasher::operator() (const ProductionRule& rule) const {
	return std::hash<std::string>()(rule.getLeft().getName());
}

// Grammar

Grammar::Grammar() {
}
