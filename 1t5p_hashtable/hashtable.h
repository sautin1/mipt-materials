#pragma once
#include <list>
#include <vector>
#include <iostream>
#include <algorithm>
#include <stdexcept>
#include <string>

template <class KeyT, class ValueT>
class HashTable {
public:
	typedef std::pair<KeyT, ValueT> TListElement;
	
	class Iterator
	{
	public:

		Iterator(const HashTable& parent, const Iterator& copy_it): parent_(parent), bucket_it_(copy_it.bucket_it_), list_it_(copy_it.list_it_){}

		Iterator(const HashTable& parent, 
				 const typename std::vector< std::list <TListElement> >::iterator& bucket_it, 
				 const typename std::list<TListElement>::iterator& list_it)
				: parent_(parent), bucket_it_(bucket_it), list_it_(list_it){}

		TListElement* operator->()
		{
			return &(*list_it_);
		}

		const TListElement* operator->() const
		{
			return &(*list_it_);
		}

		const ValueT& operator*() const
		{
			return list_it_->second;
		}
		ValueT& operator*()
		{
			return list_it_->second;
		}

		Iterator operator=(const Iterator& copy_it)
		{
			//Iterator it(parent_, copy_it.bucket_it_, copy_it.list_it_);
			this->bucket_it_ = copy_it.bucket_it_;
			this->list_it_ = copy_it.list_it_;
			return *this;
		}

		Iterator& operator++()
		{
			++list_it_;
			if (list_it_ == bucket_it_->end()){
				if (bucket_it_ == parent_.end().bucket_it_){
					//*this points to the end of hashtable_
					return *this;
				}
				++bucket_it_;
				while (bucket_it_ != parent_.end().bucket_it_ && bucket_it_->empty()){
					++bucket_it_;
				}
				if (bucket_it_->empty()){
					list_it_ = parent_.end().list_it_;
				}
				else {
					list_it_ = (*bucket_it_).begin();
				}
			}
			return *this;
		} //prefix form

		Iterator& operator--()
		{
			if (list_it_ == bucket_it_->begin()){
				if (bucket_it_ == parent_.begin().bucket_it_){
					//*this points to the begin of hashtable_
					return *this;
				}
				--bucket_it_;
				while (bucket_it_ != parent_.begin().bucket_it_ && bucket_it_->empty()){
					--bucket_it_;
				}
				if (bucket_it_->empty()){
					list_it_ = parent_.begin().list_it_;
				}
				else {
					list_it_ = (*bucket_it_).end();
				}
			}
			else {
				--list_it_;
			}
			return *this;
		} //prefix form
		
		const Iterator operator++(int)
		{
			Iterator old = *this;
			operator ++();
			return old;
		} //postfix form

		const Iterator operator--(int)
		{
			Iterator old = *this;
			operator --();
			return old; 
		} //postfix form

		bool operator==(const Iterator &equal_it) const
		{
			return (list_it_ == equal_it.list_it_) && (bucket_it_ == equal_it.bucket_it_);
		}

		bool operator!=(const Iterator &inequal_it) const
		{
			return !((*this) == inequal_it);
			//return (list_it_ != inequal_it.list_it_) || (bucket_it_ != inequal_it.bucket_it_);
		}

		const HashTable& parent_;
		typename std::vector< std::list <TListElement> >::iterator bucket_it_;
		typename std::list<TListElement>::iterator list_it_;
	};

	HashTable(size_t buckets_quantity)
		:hashtable_(buckets_quantity, std::list<TListElement>()),
		  it_begin_(*this, hashtable_.begin(), hashtable_.front().begin()),
		  it_end_(*this, hashtable_.end(), hashtable_.back().end()),
		  element_quantity_(0)
	{
		/*Creates hashtable_ with buckets_quantity empty lists.
		Complexity: linear in the buckets_quantity*/
		update_begin();
		update_end();
	}

	Iterator begin() const
	{
		return it_begin_;
	}

	Iterator end() const
	{
		return it_end_;
	}

	bool isEmpty() const
	{
		return (element_quantity_ == 0);
	}


	Iterator find(const KeyT& key) const
	{
		/*Finds key in hashtable_
		Complexity: linear in the element_quantity_*/
		if (isEmpty()){
			return it_end_;
		}
		Iterator it(*this, it_begin_);
		for (; it != it_end_; ++it){
			if (it->first == key){
				break;
			}
		}
		return it;
	}

	std::pair<Iterator, bool> insert(const KeyT& key, const ValueT& value)
	{
		/*Insert pair {key,value} into hashtable_
		Complexity: linear in the element_quantity_*/
		Iterator new_place_it = it_end_;
		if (!isEmpty()){
			new_place_it = find(key);
			/*not found => it = it_end_*/
		}
		bool success = (new_place_it == it_end_);
		if (success){
			//Element will be inserted
			size_t new_index = hash_function(key);
			hashtable_[new_index].push_front(std::make_pair(key, value));
			++element_quantity_;
			Iterator temp_it(*this, hashtable_.begin() + new_index, hashtable_[new_index].begin());
			new_place_it = temp_it;
			update_begin();
			update_end();
			if (element_quantity_ >= rehash_coefficient * hashtable_.size()){
				rehash(hashtable_.size() + 1);
			}
		}
		return std::make_pair(new_place_it, success);
	}

	Iterator erase(const Iterator erase_it)
	{
		/*Erases an element pointed by erase_it
		Complexity: constant*/
		Iterator next_element_it = erase_it;
		++next_element_it;
		bool last_element = (next_element_it == it_end_);
		(*erase_it.bucket_it_).erase(erase_it.list_it_);
		element_quantity_--;
		update_begin();
		update_end();
		if (last_element){
			next_element_it = it_end_;
		}
		return next_element_it;
	}

	Iterator erase(const KeyT& key)
	{
		/*Erases an element by its key
		Complexity: linear in the element_quantity_*/
		Iterator it(*this, it_end_);
		if (!isEmpty()){
			it = find(key);
			if (it != it_end_){
				it = erase(it);
			}
		}
		return it;
	}

	void rehash(size_t new_hashtable_size)
	{
		/*Reallocates the whole hashtable_, extending its size to new_hashtable_size
		Complexity: linear in the element_quantity_*/
		if (new_hashtable_size == hashtable_.size()){
			return;
		} else {
			if (new_hashtable_size > hashtable_.size()){
				/*run through all the elements and reallocate them*/
				update(new_hashtable_size);
			}
			/*else do nothing*/
		}
	}

	ValueT& operator[](KeyT& key)
	{
		Iterator it = find(key);
		return (*it);
	}

	size_t size()
	{
		return element_quantity_;
	}

private:

	size_t hash_function(const KeyT& key) const
	{
		return key.size() % hashtable_.size();
	}

	void update_begin()
	{
		if (isEmpty()){
			it_begin_.bucket_it_ = hashtable_.begin();
			it_begin_.list_it_ = hashtable_.front().begin();
		}
		else {
			for (size_t index = 0; index < hashtable_.size(); ++index){
				if (!hashtable_[index].empty()){
					it_begin_.bucket_it_ = hashtable_.begin() + index;
					it_begin_.list_it_ = hashtable_[index].begin();
					break;
				}
			}
		}
	}

	void update_end()
	{
		if (isEmpty()){
			it_end_ = it_begin_;
		}
		else {
			for (ssize_t index = hashtable_.size() - 1; index >= 0; --index){
				if (!hashtable_[index].empty()){
					it_end_.bucket_it_ = hashtable_.begin() + index;
					it_end_.list_it_ = hashtable_[index].end();
					break;
				}
			}
		}
	}

	void update(size_t new_hashtable_size)
	{
		/*Updates the hashtable_ after the increase of number of buckets
		Complexity: linear in the element_quantity_*/
		HashTable<KeyT, ValueT> new_hashtable(new_hashtable_size);
		
		Iterator it(*this, it_begin_);
		for (; it != it_end_; ++it){
			new_hashtable.insert(it->first, it->second);
		}
		std::swap(*this, new_hashtable);
		update_begin();
		update_end();
	}

	std::vector< std::list <TListElement> > hashtable_;
	Iterator it_begin_;
	Iterator it_end_;
	size_t element_quantity_;
	static const size_t rehash_coefficient = 4;
};
