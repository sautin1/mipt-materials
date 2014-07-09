#ifndef BINOMIAL_HEAP_H
#define BINOMIAL_HEAP_H

#include <cstddef>
#include <vector>
#include <memory>
#include <algorithm>
#include <stdexcept>

struct node
{
	node();
	node(std::shared_ptr<node> new_parent, std::shared_ptr<node> new_brother, std::shared_ptr<node> new_son, const int &new_value);
	std::shared_ptr<node> parent;
	std::shared_ptr<node> brother; //right brother
	std::shared_ptr<node> son; //left son
	int value;
};

const std::shared_ptr<node> nullshptr(nullptr);

class binomial_tree
{
public:
	std::shared_ptr<node> root_;
	binomial_tree();
	binomial_tree(std::shared_ptr<node> new_root);
	void clear();
	bool meld(binomial_tree& new_tree);
};

class binomial_heap
{
private:
	std::vector<binomial_tree> trees_;
	size_t min_;
	size_t size_;
	void heapify_up(std::shared_ptr<node>& node_ptr);
	void heapify_down(std::shared_ptr<node>& node_ptr);
	void update_min();
public:
	binomial_heap();
	binomial_heap(std::shared_ptr<node>& new_value_shptr);
	size_t size() const;
	int get_min() const;
	int extract_min();
	void meld(binomial_heap& right);
	std::shared_ptr<node> insert(const int& new_value);
	void decrease_key(std::shared_ptr<node>& change_node_ptr, const int& new_value);
	void increase_key(std::shared_ptr<node>& change_node_ptr, const int& new_value);
	void change_key  (std::shared_ptr<node>& change_node_ptr, const int& new_value);
	void erase	   (std::shared_ptr<node>&  erase_node_ptr);
};

#endif // BINOMIAL_HEAP_H
