#pragma once
#include <iostream>
#include <forward_list>
#include <functional>
#include <utility>

const size_t INITIAL_BUCKET_COUNT = 1;
const size_t GROWTH_FACTOR = 2;
const double MAX_LOAD_FACTOR = 0.75;

template <typename Key, typename Value, typename Hash, typename KeyEqual>
class HashTable;
template <typename Key, typename Value, typename Hash, typename KeyEqual>
class TableIterator;
template <typename Key, typename Value, typename Hash, typename KeyEqual>
class ConstTableIterator;

template <typename Key, typename Value, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
class TableIterator {
public:
    TableIterator(HashTable<Key, Value, Hash, KeyEqual>& table, 
                  size_t bucket, 
                  typename std::forward_list<std::pair<Key, Value>>::iterator it)
        : table_{table}, bucket_{bucket}, it_{it} {
        while (bucket_ < table_.buckets_count_ && it_ == table_.buckets_[bucket_].end()) {
            ++bucket_;
            it_ = table_.buckets_[bucket_].begin();
        } 
    };

    std::pair<Key, Value>& operator*() {
        return *it_;
    }
    std::pair<Key, Value>* operator->() {
        return &(*it_);
    }

    void operator++() {
        ++it_;
        while (bucket_ < table_.buckets_count_ && it_ == table_.buckets_[bucket_].end()) {
            ++bucket_;
            it_ = table_.buckets_[bucket_].begin();
        }
    } 
    bool operator==(const TableIterator& another) const {
        return (bucket_ == another.bucket_ && it_ == another.it_);
    }
    bool operator!=(const TableIterator& another) const {
        return !(*this == another);
    }

private:
    HashTable<Key, Value, Hash, KeyEqual>& table_;
    size_t bucket_;
    typename std::forward_list<std::pair<Key, Value>>::iterator it_;
};

template <typename Key, typename Value, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
class ConstTableIterator {
public:
    ConstTableIterator(const HashTable<Key, Value, Hash, KeyEqual>& table, 
                  size_t bucket, 
                  typename std::forward_list<std::pair<Key, Value>>::const_iterator it)
        : table_{table}, bucket_{bucket}, it_{it} {
        while (bucket_ < table_.buckets_count_ && it_ == table_.buckets_[bucket_].end()) {
            ++bucket_;
            it_ = table_.buckets_[bucket_].begin();
        }            
    };
    const std::pair<Key, Value>& operator*() {
        assert(it_ != table_.buckets_[bucket_].end());
        return *it_;
    }
    const std::pair<Key, Value>* operator->() const {
        return &(*it_);
    }
    void operator++() {
        ++it_;
        while (bucket_ < table_.buckets_count_ && it_ == table_.buckets_[bucket_].end()) {
            ++bucket_;
            it_ = table_.buckets_[bucket_].begin();
        }
    }
    bool operator==(const ConstTableIterator& another) const {
        return (bucket_ == another.bucket_ && it_ == another.it_);
    }
    bool operator!=(const ConstTableIterator& another) const {
        return !(*this == another);
    }

private:
    const HashTable<Key, Value, Hash, KeyEqual>& table_;
    size_t bucket_;
    typename std::forward_list<std::pair<Key, Value>>::const_iterator it_;
};

template <typename Key, typename Value, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
class HashTable {
public:
    HashTable() : buckets_(INITIAL_BUCKET_COUNT + 1) {
        buckets_count_ = INITIAL_BUCKET_COUNT;
        elements_ = 0;
    }

    typedef TableIterator<Key, Value, Hash, KeyEqual> TableIt;
    typedef ConstTableIterator<Key, Value, Hash, KeyEqual> ConstTableIt;

    HashTable(const HashTable<Key, Value, Hash, KeyEqual>& another) = default;
    HashTable(HashTable<Key, Value, Hash, KeyEqual>&& another) = default;
    HashTable& operator=(const HashTable<Key, Value, Hash, KeyEqual>& another) = default;
    HashTable& operator=(HashTable<Key, Value, Hash, KeyEqual>&& another) = default;

    Value& operator[](const Key& k) {
        Value* found = FindByKey(k);
        if (found == nullptr) {
            CreateKey(k);
            return *FindByKey(k);
        } else {
            return *found;
        }
    }
    Value at(Key k) const {
        const Value* found = FindByKey(k);
        if (found == nullptr) {
            throw std::out_of_range{"Key not found!"};
        }
        return *found;
    };
    
    TableIt find(Key to_find) {
        size_t hash = hasher_(to_find) % buckets_count_;
        for (auto it = buckets_[hash].begin(); it != buckets_[hash].end(); ++it) {
            if (equal_checker_(it->first, to_find)) {
                return TableIterator(*this, hash, it);
            }
        }
        return end();
    }
    ConstTableIt find(Key to_find) const {
        size_t hash = hasher_(to_find) % buckets_count_;
        for (auto it = buckets_[hash].begin(); it != buckets_[hash].end(); ++it) {
            if (equal_checker_(it->first, to_find)) {
                return ConstTableIterator(*this, hash, it);
            }
        }
        return end();
    }

    TableIt begin() {
        return TableIt(*this, 0, buckets_[0].begin());
    }

    ConstTableIt begin() const {
        return ConstTableIt(*this, 0, buckets_[0].begin());
    }

    TableIt end() {
        return TableIt(*this, buckets_count_, buckets_[buckets_count_].begin());
    }
    
    ConstTableIt end() const {
        return ConstTableIt(*this, buckets_count_, buckets_[buckets_count_].begin());
    }

    std::pair<TableIt, bool> insert(const std::pair<Key, Value>& key_value) {
        auto k = key_value.first;
        auto v = key_value.second;
        auto it = find(k);
        if (it == end()) {
            CreateKey(k);
            *FindByKey(k) = v;    
            return {it, true};
        }
        return {it, false};
    }
    
    std::pair<TableIt, bool> emplace(Key&& k, Value&& v) {
        auto it = find(k);
        if (it == end()) {
            CreateKey(k);
            *FindByKey(k) = v;    
            return {it, true};
        }
        return {it, false};
    }

    void clear() {
        buckets_count_ = INITIAL_BUCKET_COUNT;
        buckets_ = std::vector<std::forward_list<std::pair<Key, Value>>>(INITIAL_BUCKET_COUNT+1);
        elements_ = 0;
        buckets_.clear();
    }

    size_t size() const {
        return elements_;
    } 

    bool empty() const {
        return (elements_ == 0);
    }
 
private:
    std::vector<std::forward_list<std::pair<Key, Value>>> buckets_;
    size_t elements_;
    Hash hasher_;
    KeyEqual equal_checker_;
    size_t buckets_count_;
    friend class TableIterator<Key, Value, Hash, KeyEqual>;
    friend class ConstTableIterator<Key, Value, Hash, KeyEqual>;

    double LoadFactor() const {
        return elements_ / static_cast<double>(buckets_count_);
    }

    const Value* FindByKey(const Key& to_find) const {
        size_t hash = hasher_(to_find) % buckets_count_;
        for (auto& [key, value] : buckets_[hash]) {
            if (equal_checker_(key, to_find)) {
                return &value;
            }
        }
        return nullptr;
    }

    Value* FindByKey(const Key& to_find) {
        size_t hash = hasher_(to_find) % buckets_count_;
        for (auto& [key, value] : buckets_[hash]) {
            if (equal_checker_(key, to_find)) {
                return &value;
            }
        }
        return nullptr;
    }

    void CreateKey(const Key& to_create) {
        ++elements_;
        if (LoadFactor() > MAX_LOAD_FACTOR) {
            Grow();
        }
        size_t hash = hasher_(to_create) % buckets_count_;
        buckets_[hash].push_front(std::pair<Key, Value>{to_create, Value{}});
    }

    void Grow() {
        buckets_count_ *= GROWTH_FACTOR;
        std::vector<std::forward_list<std::pair<Key, Value>>> new_buckets(buckets_count_ * GROWTH_FACTOR + 1);
        for (size_t i = 0; i < buckets_count_; ++i) {
            for (auto& key_value : buckets_[i]) {
                size_t hash = hasher_(key_value.first) % buckets_count_;
                new_buckets[hash].push_front(std::move(key_value));
            }
        }
        buckets_ = std::move(new_buckets);
    }
};
