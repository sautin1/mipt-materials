#pragma once
#include <list>
#include <vector>
#include <iostream>
#include <algorithm>

template <class KeyT, class ValueT>
class HashTable {
public:
	typedef std::pair<const KeyT, ValueT> TListElement;
	
	class Iterator
	{
	public: 
		//Iterator(){}

        Iterator(const HashTable& parent, const Iterator& copy_it): parent_(parent), bucket_it_(copy_it.bucket_it_), list_it_(copy_it.list_it_){}

		Iterator(const HashTable& parent, 
				 const typename std::vector< std::list <TListElement> >::iterator& bucket_it, 
				 const typename std::list<TListElement>::iterator& list_it)
				: parent_(parent), bucket_it_(bucket_it), list_it_(list_it){}

        TListElement* operator ->()
		{
            return &(*list_it_);
		}

		operator const Iterator* ()
		{
			return this;
		}

        ValueT& operator *() const {
            return list_it_->second;
		}
		
		Iterator operator =(const Iterator& copy_it)
		{
			Iterator it;
			it.list_it_ = copy_it.list_it_;
			it.bucket_it_ = copy_it.bucket_it_;
			return it;
		}

		Iterator& operator ++()
		{
			++list_it_;
			while (list_it_ == bucket_it_ -> end()){
				++bucket_it_;
				list_it_ = bucket_it_ -> begin();
			}
			return *this;
		} //prefix form
		
		Iterator& operator --()
		{
			--list_it_;
			while (list_it_ == bucket_it_ -> begin()){
				--bucket_it_;
				list_it_ = bucket_it_ -> end();
			}
			return *this;
		} //prefix form
		
		const Iterator operator ++(int)
		{
			Iterator old = *this;
			operator ++();
			return old;
		} //postfix form

		const Iterator operator --(int)
		{
			Iterator old = *this;
			operator --();
			return old; 
		} //postfix form

		bool operator == (const Iterator &equal_it)
		{
			return (list_it_ == equal_it.list_it_) && (bucket_it_ == equal_it.bucket_it_);
		}

		bool operator != (const Iterator &inequal_it)
		{
            return (list_it_ != inequal_it.list_it_) || (bucket_it_ != inequal_it.bucket_it_);
		}

        const HashTable& parent_;
        typename std::vector< std::list <TListElement> >::iterator bucket_it_;
        typename std::list<TListElement>::iterator list_it_;
	};

	HashTable(size_t buckets_quantity)
    :hashtable_(buckets_quantity, std::list<TListElement>()),
      it_begin_(*this, hashtable_.begin(), hashtable_.front().begin()),
      it_end_(*this, hashtable_.end(), hashtable_.back().end())
	{
		/*Creates hashtable_ with buckets_quantity empty lists.
		Complexity: linear in the buckets_quantity*/
		update_begin();
		update_end();
	}

	~HashTable(){}

	Iterator begin() const
	{
		return it_begin_;
	}

	Iterator end() const
	{
		return it_end_;
	}

	Iterator find(const KeyT& key) const
	{
		/*Finds key in hashtable_
		Complexity: linear in the element_quantity_*/
        Iterator it(*this, it_begin_);
        for (; it != it_end_; ++it){
            if ((*it) == key){
				break;
			}
		}
		return it;
	}

	std::pair<Iterator, bool> insert(const KeyT& key, const ValueT& value)
	{
		/*Insert pair {key,value} into hashtable_
		Complexity: linear in the element_quantity_*/
		Iterator new_place_it = find(key);
		if (new_place_it != it_end_){
			/*found => cannot be inserted*/
		} else {
			/*not found => will be inserted*/
			size_t new_index = hash_function(key);
			hashtable_[new_index].push_front(std::make_pair(key, value));
			++element_quantity_;
			if (element_quantity_ >= rehash_coefficient * hashtable_.size()){
				rehash(hashtable_.size() + 1);
			}
			update_end();
		}
		return std::make_pair(new_place_it, new_place_it == it_end_);
	}

	Iterator erase(const Iterator erase_it)
	{
		/*Erases an element pointed by erase_it
		Complexity: constant*/
		size_t erase_list_index = hash_function((*erase_it).first);
		Iterator next_element_it = hashtable_[erase_list_index].erase(erase_it);
		element_quantity_--;
		update_begin();
		update_end();
		return next_element_it;
	}

	Iterator erase(const KeyT& key)
	{
		/*Erases an element by its key
		Complexity: linear in the element_quantity_*/
		Iterator it = find(key);
		if (it != it_end_){
			it = erase(it);
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
                for (size_t new_bucket = hashtable_.size(); new_bucket < new_hashtable_size; ++new_bucket){
                    //std::list<TListElement> empty_list;
//                    hashtable_.push_back(std::list<TListElement>());
                hashtable_.resize(new_hashtable_size); //hashtable_.size() has changed
                }
				/*run through all the elements and reallocate them*/
				update();
			}
			/*else do nothing*/
		}
		update_begin();
		update_end();
	}

	ValueT& operator [](KeyT& key)
	{
		Iterator it = find(key);
		return (*it);
	}

private:
	/*size_t hash_function(KeyT key);*/

	size_t hash_function(const KeyT& key) const
	{
		return key.size() % hashtable_.size();
	}

	void update_begin()
	{
        for (it_begin_.list_it_ = hashtable_.front().begin(); it_begin_.list_it_ != hashtable_.back().end(); ++it_begin_){
			if (it_begin_.list_it_ != it_begin_.bucket_it_->end()){
				break;
			}
		}
	}

	void update_end()
	{
		for (it_end_.list_it_ = hashtable_.back().end(); it_end_.list_it_ != hashtable_.front().begin(); --it_end_){
			if (it_end_.list_it_ != it_end_.bucket_it_->begin()){
				break;
			}
		}
    }

	void update()
	{
		/*Updates the hashtable_ after the increase of number of buckets
		Complexity: linear in the element_quantity_*/
		std::vector< std::list <TListElement> > new_hashtable;
		std::swap(hashtable_, new_hashtable);
		
		element_quantity_ = 0;
		for (Iterator it = it_begin_; it != it_end_; ++it){
			insert(it->first, it->second);
		}
    }

    size_t element_quantity_;
    std::vector< std::list <TListElement> > hashtable_;
    Iterator it_begin_;
    Iterator it_end_;
	static const size_t rehash_coefficient = 4;
};
