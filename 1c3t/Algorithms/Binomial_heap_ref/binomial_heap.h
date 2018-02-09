#ifndef BINOMIAL_HEAP_H
#define BINOMIAL_HEAP_H

#include <cstddef>
#include <vector>
#include <memory>
#include <algorithm>
#include <stdexcept>

template <typename keyT, typename comparatorT>
class binomial_tree
{
    template <typename keyT1, typename comparatorT1> friend class binomial_heap;
private:
    typedef std::shared_ptr< binomial_tree<keyT, comparatorT> > binomial_tree_ptr;
    binomial_tree_ptr parent_tree_ptr_;
    binomial_tree_ptr brother_tree_ptr_;
    binomial_tree_ptr son_tree_ptr_;
    ssize_t power_;
    keyT root_key_;
public:
    binomial_tree()
        :parent_tree_ptr_(nullptr), brother_tree_ptr_(nullptr), son_tree_ptr_(nullptr), power_(-1)
    {}

    binomial_tree(const keyT& new_key)
        :parent_tree_ptr_(nullptr), brother_tree_ptr_(nullptr), son_tree_ptr_(nullptr), power_(0), root_key_(new_key)
    {}


    ssize_t power() const
    {
        return power_;
    }

    size_t size() const
    {
        return std::pow(2, power());
    }

    void isolate()
    {
        parent_tree_ptr_.reset();
        brother_tree_ptr_.reset();
    }

    void clear()
    {
        isolate();
        son_tree_ptr_.reset();
        power_ = -1;
    }

    template <typename keyT1, typename comparatorT1>
    friend void meld_trees(std::shared_ptr< binomial_tree<keyT1, comparatorT1> >& left_tree, std::shared_ptr< binomial_tree<keyT1, comparatorT1> >& right_tree);
};

template <typename keyT, typename comparatorT>
void meld_trees(std::shared_ptr< binomial_tree<keyT, comparatorT> >& left_tree, std::shared_ptr< binomial_tree<keyT, comparatorT> >& right_tree)
{
    if (left_tree->power() != right_tree->power() && left_tree->power() != -1 && right_tree->power() != -1){
        throw std::logic_error("Meld(): try to meld trees of different powers");
    }
    if (left_tree->power() == -1){
        left_tree = right_tree;
        return;
    }
    if (right_tree->power() == -1){
        return;
    }
    comparatorT isless;
    if (isless(right_tree->root_key_, left_tree->root_key_)){
        std::swap(left_tree, right_tree);
    }
    right_tree-> parent_tree_ptr_ = left_tree;
    right_tree->brother_tree_ptr_ = left_tree->son_tree_ptr_;
    left_tree ->    son_tree_ptr_ = right_tree;
    ++left_tree->power_;
}

template <typename keyT, typename comparatorT>
class binomial_heap
{
private:
    typedef std::shared_ptr< binomial_tree<keyT, comparatorT> > binomial_tree_ptr;
    std::vector< binomial_tree_ptr > ptr_forest_;
    binomial_tree_ptr min_tree_ptr_;
    size_t size_;
    comparatorT isless_;

    binomial_heap(binomial_tree_ptr& new_binomial_tree)
        :ptr_forest_(new_binomial_tree->power(), binomial_tree_ptr(nullptr)), min_tree_ptr_(new_binomial_tree),
          size_(std::pow(2, new_binomial_tree->power()))
    {
        ptr_forest_.push_back(new_binomial_tree);
    }

    void update_min()
    {
        for (size_t tree_index = 0; tree_index < ptr_forest_.size(); ++tree_index){
            if (ptr_forest_[tree_index] != binomial_tree_ptr(nullptr) &&
               (min_tree_ptr_ == binomial_tree_ptr(nullptr) || isless_(ptr_forest_[tree_index]->root_key_, min_tree_ptr_->root_key_))){
                min_tree_ptr_ = ptr_forest_[tree_index];
            }
        }
    }

    void erase_root(binomial_tree_ptr& erase_tree_root)
    {
        while (erase_tree_root->parent_tree_ptr_ != binomial_tree_ptr(nullptr)){
            erase_tree_root = erase_tree_root->parent_tree_ptr_;
        }
        binomial_heap<keyT, comparatorT> son_heap;

        binomial_tree_ptr son_tree_ptr = erase_tree_root->son_tree_ptr_;
        while (son_tree_ptr != binomial_tree_ptr(nullptr)){
            son_heap.ptr_forest_.push_back(son_tree_ptr);
            son_tree_ptr = son_tree_ptr->brother_tree_ptr_;
            son_heap.ptr_forest_.back()->isolate();
            size_t son_tree_size = son_heap.ptr_forest_.back()->size();
            size_ -= son_tree_size;
            son_heap.size_ += son_tree_size;
        }
        std::reverse(son_heap.ptr_forest_.begin(), son_heap.ptr_forest_.end());
        ptr_forest_[erase_tree_root->power()] = std::make_shared< binomial_tree<keyT, comparatorT> >();
        --size_;
        meld(son_heap);
        //min_tree_ptr_ was updated in meld()
    }

    void make_root(binomial_tree_ptr& tree_ptr)
    {
        while (tree_ptr->parent_tree_ptr_ != binomial_tree_ptr(nullptr)){
            std::swap(tree_ptr->root_key_, tree_ptr->parent_tree_ptr_->root_key_);
            tree_ptr = tree_ptr->parent_tree_ptr_;
        }
    }

    void decrease_key(binomial_tree_ptr& change_tree_ptr, const keyT& new_key)
    {
        change_tree_ptr->root_key_ = new_key;
        while (change_tree_ptr->parent_tree_ptr_ != binomial_tree_ptr(nullptr)
               && isless_(new_key, change_tree_ptr->parent_tree_ptr_->root_key_)){
            std::swap(change_tree_ptr->root_key_, change_tree_ptr->parent_tree_ptr_->root_key_);
            change_tree_ptr = change_tree_ptr->parent_tree_ptr_;
        }
        //update min_tree_ptr_
        if (change_tree_ptr->parent_tree_ptr_ == binomial_tree_ptr(nullptr)){
            //we reached the root of a binomial tree => heap minimum may have changed
            if (isless_(new_key, min_tree_ptr_->root_key_)){
                min_tree_ptr_ = change_tree_ptr;
            }
        }
    }

    void increase_key(binomial_tree_ptr& change_tree_ptr, const keyT& new_key)
    {
        erase(change_tree_ptr);
        insert(new_key);
    }
public:
    binomial_heap()
        :size_(0)
    {}

    size_t size() const
    {
        return size_;
    }

    keyT get_min() const
    {
        return min_tree_ptr_->root_key_;
    }

    keyT extract_min()
    {
        keyT min = get_min();
        erase_root(min_tree_ptr_);
        //min_tree_ptr_ was updated in erase_root()
        return min;
    }

    void meld(binomial_heap<keyT, comparatorT>& right_heap)
    {
        size_t new_forest_size = std::max(ptr_forest_.size(), right_heap.ptr_forest_.size());

        min_tree_ptr_.reset();
        binomial_tree_ptr tmp_tree_ptr = std::make_shared< binomial_tree<keyT, comparatorT> >();
        for (ssize_t tree_index = 0; tree_index < new_forest_size; ++tree_index){
            //resize forests
            if (tree_index >= ptr_forest_.size()){
                ptr_forest_.push_back(std::make_shared< binomial_tree<keyT, comparatorT> > ());
            }
            if (tree_index >= right_heap.ptr_forest_.size()){
                right_heap.ptr_forest_.push_back(std::make_shared< binomial_tree<keyT, comparatorT> > ());
            }

            meld_trees<keyT, comparatorT>(ptr_forest_[tree_index], right_heap.ptr_forest_[tree_index]);
            if (ptr_forest_[tree_index]->power() > tree_index){
                std::swap(tmp_tree_ptr, ptr_forest_[tree_index]);
            } else {
                meld_trees<keyT, comparatorT>(ptr_forest_[tree_index], tmp_tree_ptr);
                if (ptr_forest_[tree_index]->power() > tree_index){
                    tmp_tree_ptr = ptr_forest_[tree_index];
                    ptr_forest_[tree_index] = std::make_shared< binomial_tree<keyT, comparatorT> > ();
                } else {
                    tmp_tree_ptr = std::make_shared< binomial_tree<keyT, comparatorT> > ();
                }
            }
            //update min_tree_ptr_
            if (ptr_forest_[tree_index]->power() != -1 &&
               (min_tree_ptr_ == binomial_tree_ptr(nullptr) || isless_(ptr_forest_[tree_index]->root_key_, min_tree_ptr_->root_key_))){
                min_tree_ptr_ = ptr_forest_[tree_index];
            }
        }
        if (tmp_tree_ptr->power() != -1){
            ptr_forest_.push_back(tmp_tree_ptr);
            if (min_tree_ptr_ == binomial_tree_ptr(nullptr) || isless_(tmp_tree_ptr->root_key_, min_tree_ptr_->root_key_)){
                min_tree_ptr_ = tmp_tree_ptr;
            }
        }
        size_ += right_heap.size();
    }

    binomial_tree_ptr insert(const keyT& new_key)
    {
        binomial_tree_ptr new_tree_ptr = std::make_shared< binomial_tree<keyT, comparatorT> >(new_key);
        binomial_heap new_heap(new_tree_ptr);
        meld(new_heap);
        //min_tree_ptr_ was updated in meld()
        return new_tree_ptr;
    }

    void erase(binomial_tree_ptr& erase_tree_ptr)
    {
        make_root(erase_tree_ptr);
        erase_root(erase_tree_ptr);
        //min_tree_ptr_ was updated in erase_root()
    }

    void change_key(binomial_tree_ptr& change_tree_ptr, const keyT& new_key)
    {
        if (isless_(new_key, change_tree_ptr->root_key_)){
            decrease_key(change_tree_ptr, new_key);
            //min_tree_ptr_ was updated in decrease_key()
        } else if (isless_(change_tree_ptr->root_key_, new_key)){
            increase_key(change_tree_ptr, new_key);
            //min_tree_ptr_ was updated in increase_key()
        }
    }
};

#endif // BINOMIAL_HEAP_H
