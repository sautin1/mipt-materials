#include "binomial_heap.h"

node::node()
    :parent(nullshptr), brother(nullshptr), son(nullshptr), value()
{}

node::node(std::shared_ptr<node> new_parent, std::shared_ptr<node> new_brother, std::shared_ptr<node> new_son, const double& new_value)
    :parent(new_parent), brother(new_brother), son(new_son), value(new_value)
{}

binomial_tree::binomial_tree()
    :root_(nullshptr)
{}

binomial_tree::binomial_tree(std::shared_ptr<node> new_root)
    :root_(new_root)
{}

void binomial_tree::clear()
{
    root_ = nullshptr;
}

bool binomial_tree::meld(binomial_tree& new_tree)
{
    if (new_tree.root_ == nullshptr){
        return false;
    }
    if (root_ == nullshptr){
        *this = new_tree;
        return false;
    }
    if (new_tree.root_->value < root_->value){
        std::swap(*this, new_tree);
    }
    new_tree.root_->parent = root_;
    new_tree.root_->brother = root_->son;
    root_->son = new_tree.root_;
    return true;
}

binomial_heap::binomial_heap()
    :trees_(), min_(1000), size_(0)
{}

binomial_heap::binomial_heap(std::shared_ptr<node>& new_value_shptr)
    :trees_(1, new_value_shptr), min_(0), size_(1)
{}

size_t binomial_heap::size() const
{
    return size_;
}

double binomial_heap::get_min() const
{
    if (size_ == 0){
        throw std::logic_error(std::string("Heap is empty!"));
    }
    return trees_[min_].root_->value;
}

double binomial_heap::extract_min()
{
    double min_value = get_min();

    binomial_heap son_heap;
    std::shared_ptr<node> min_tree_son = trees_[min_].root_->son;
    while (min_tree_son != nullshptr){
        son_heap.trees_.push_back(binomial_tree(min_tree_son));
        min_tree_son = min_tree_son->brother;
        son_heap.trees_.back().root_->brother = nullshptr;
        son_heap.trees_.back().root_->parent  = nullshptr;
    }
    std::reverse(son_heap.trees_.begin(), son_heap.trees_.end());
    son_heap.size_ = std::pow(2, min_) - 1;
    son_heap.update_min();

    trees_[min_].root_ = nullshptr;
    size_ -= son_heap.size() + 1;
    meld(son_heap);
    update_min();
    return min_value;
}

void binomial_heap::update_min()
{
    for (size_t tree_index = 0; tree_index < trees_.size(); ++tree_index){
        if (trees_[tree_index].root_ != nullshptr &&
        (trees_[min_].root_ == nullshptr || trees_[tree_index].root_->value < trees_[min_].root_->value)){
            min_ = tree_index;
        }
    }
}

void binomial_heap::meld(binomial_heap& new_heap)
{
    if (new_heap.size() == 0){
        return;
    }
    if (size_ == 0){
        std::swap(*this, new_heap);
        update_min();
        return;
    }

    binomial_tree tmp_tree;
    trees_.resize(std::max(trees_.size(), new_heap.trees_.size()), binomial_tree());
    new_heap.trees_.resize(std::max(trees_.size(), new_heap.trees_.size()), binomial_tree());
    for (size_t tree_index = 0; tree_index < trees_.size(); ++tree_index){
        if (trees_[tree_index].meld(new_heap.trees_[tree_index])){
            std::swap(tmp_tree, trees_[tree_index]);
        } else {
            if (trees_[tree_index].meld(tmp_tree)){
                tmp_tree = trees_[tree_index];
                trees_[tree_index].clear();
            } else {
                tmp_tree.clear();
            }
        }
    }
    if (tmp_tree.root_ != nullshptr){
        trees_.push_back(tmp_tree);
    }
    size_ += new_heap.size();
    update_min();
}

std::shared_ptr<node> binomial_heap::insert(const double& new_value)
{
    std::shared_ptr<node> new_value_shptr(new node(nullshptr, nullshptr, nullshptr, new_value));
    binomial_heap new_heap(new_value_shptr);
    meld(new_heap);
    return new_value_shptr;
}

void binomial_heap::heapify_up(std::shared_ptr<node>& node_ptr)
{
    while (node_ptr->parent != nullshptr && node_ptr->value < node_ptr->parent->value){
        std::swap(node_ptr->value, node_ptr->parent->value);
        node_ptr = node_ptr->parent;
    }
}

void binomial_heap::decrease_key(std::shared_ptr<node>& change_node_ptr, const double& new_value)
{
    change_node_ptr->value = new_value;
    heapify_up(change_node_ptr);
}

void binomial_heap::increase_key(std::shared_ptr<node>& change_node_ptr, const double& new_value)
{
    erase(change_node_ptr);
    insert(new_value);
}

void binomial_heap::change_key(std::shared_ptr<node>& change_node_ptr, const double& new_value)
{
    if (new_value < change_node_ptr->value){
        decrease_key(change_node_ptr, new_value);
    } else if (new_value > change_node_ptr->value){
        increase_key(change_node_ptr, new_value);
    } else return;
    update_min();
}

void binomial_heap::erase(std::shared_ptr<node>& erase_node_ptr)
{
    decrease_key(erase_node_ptr, get_min() - 1);
    update_min();
    extract_min();
}
