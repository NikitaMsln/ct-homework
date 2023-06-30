#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <utility>

template <typename T, size_t SMALL_SIZE>
class socow_vector {
public:
  using value_type = T;

  using reference = T&;
  using const_reference = const T&;

  using pointer = T*;
  using const_pointer = const T*;

  using iterator = pointer;
  using const_iterator = const_pointer;

public:
  socow_vector() noexcept : dynamic_array_(nullptr), size_(0), is_dynamic_(false) {}

  socow_vector(const socow_vector& other) {
    *this = other;
  }

  socow_vector& operator=(const socow_vector& other) {
    if (this == &other) {
      return *this;
    }
    if (other.is_dynamic_) {
      clear_and_convert_to_small();
      dynamic_array_ = other.dynamic_array_;
      dynamic_array_->inc_counter();
    } else if (is_dynamic_) {
      cow_array* last_data = dynamic_array_;
      try {
        std::uninitialized_copy(other.begin(), other.end(), static_array_);
      } catch (...) {
        dynamic_array_ = last_data;
        throw;
      }
      cow_array::delete_ptr(last_data, size_);
    } else if (size_ == 0) {
      std::uninitialized_copy(other.begin(), other.end(), static_array_);
    } else if (other.size_ == 0) {
      clear_and_convert_to_small();
    } else {
      size_t buffer_size = std::min(size_, other.size_);
      socow_vector tmp;

      std::uninitialized_copy_n(other.begin(), buffer_size, tmp.begin());
      tmp.size_ = buffer_size;

      std::destroy_n(static_array_ + buffer_size, size_ - buffer_size);

      std::uninitialized_copy(other.begin() + buffer_size, other.end(), static_array_ + buffer_size);
      size_ = other.size_;
      std::swap_ranges(tmp.begin(), tmp.end(), static_array_);
    }
    is_dynamic_ = other.is_dynamic_;
    size_ = other.size_;
    return *this;
  }

  ~socow_vector() noexcept {
    clear_and_convert_to_small();
  }

  reference operator[](size_t index) {
    assert(index < size_);
    return data()[index];
  }

  const_reference operator[](size_t index) const {
    assert(index < size_);
    return data()[index];
  }

  pointer data() {
    if (is_dynamic_) {
      unshare();
      return dynamic_array_->data();
    } else {
      return static_array_;
    }
  }

  const_pointer data() const noexcept {
    return is_dynamic_ ? dynamic_array_->const_data() : static_array_;
  }

  size_t size() const noexcept {
    return size_;
  }

  reference front() {
    assert(size_ > 0);
    return *begin();
  }

  const_reference front() const {
    assert(size_ > 0);
    return *begin();
  }

  reference back() {
    assert(size_ > 0);
    return *(end() - 1);
  }

  const_reference back() const {
    assert(size_ > 0);
    return *(end() - 1);
  }

  void push_back(const_reference value) {
    insert(std::as_const(*this).end(), value);
  }

  void pop_back() {
    assert(size_ > 0);
    erase(std::as_const(*this).end() - 1);
  }

  bool empty() const noexcept {
    return size_ == 0;
  }

  size_t capacity() const noexcept {
    return is_dynamic_ ? dynamic_array_->capacity() : SMALL_SIZE;
  }

  void reserve(size_t new_capacity) {
    if (new_capacity < capacity() && (!is_dynamic_ || dynamic_array_->ptr_counter() <= 1)) {
      return;
    }
    if (size_ <= new_capacity) {
      set_capacity(new_capacity);
    }
  }

  void shrink_to_fit() {
    set_capacity(size_);
  }

  void clear() {
    erase(std::as_const(*this).begin(), std::as_const(*this).end());
    size_ = 0;
  }

  void swap(socow_vector& other) {
    swap(this, &other);
  }

  iterator begin() noexcept {
    return data();
  }

  iterator end() noexcept {
    return data() + size_;
  }

  const_iterator begin() const noexcept {
    return data();
  }

  const_iterator end() const noexcept {
    return data() + size_;
  }

  iterator insert(const_iterator pos, const T& value) {
    size_t index = pos - std::as_const(*this).begin();
    if (size_ == capacity() || is_shared()) {
      socow_vector tmp(cow_array::init(std::as_const(*this).data(), size_,
                                       (capacity() > size_) ? capacity() : capacity() * 2, value, index),
                       size_ + 1);
      *this = tmp;
    } else {
      if (is_dynamic_) {
        dynamic_array_->push_back(value, size_);
      } else {
        new (end()) value_type(value);
      }
      for (size_t i = size_++; i > index; i--) {
        std::swap(operator[](i), operator[](i - 1));
      }
    }
    return begin() + index;
  }

  iterator erase(const_iterator pos) {
    return erase(pos, pos + 1);
  }

  iterator erase(const_iterator first, const_iterator last) {
    size_t count = last - first;
    size_t first_index = first - std::as_const(*this).begin();
    if (count == 0) {
      return begin() + first_index;
    }

    if (is_shared()) {
      socow_vector tmp(cow_array::init(*dynamic_array_, first_index), first_index);
      for (size_t i = first_index + count; i < size_; i++) {
        tmp.push_back(std::as_const(*this).operator[](i));
      }
      *this = tmp;
    } else {
      for (size_t from = first_index, to = from + count; to < size_; to++, from++) {
        std::swap(operator[](from), operator[](to));
      }
      size_t new_size = size_ - count;
      std::destroy(begin() + new_size, end());
      size_ = new_size;
    }
    return begin() + first_index;
  }

  bool is_shared() const noexcept {
    return is_dynamic_ && dynamic_array_->ptr_counter() > 1;
  }

private:
  void unshare() {
    if (is_shared()) {
      cow_array* tmp = cow_array::init(*dynamic_array_, size_);
      cow_array::delete_ptr(dynamic_array_, size_);
      dynamic_array_ = tmp;
    }
  }

  void clear_and_convert_to_small() noexcept {
    if (!is_dynamic_) {
      std::destroy(begin(), end());
      size_ = 0;
    } else {
      cow_array::delete_ptr(dynamic_array_, size_);
      is_dynamic_ = false;
      size_ = 0;
    }
  }

  void set_capacity(size_t new_capacity) {
    if (new_capacity == capacity()) {
      return;
    }
    if (new_capacity <= SMALL_SIZE) {
      if (is_dynamic_) {
        dynamic_to_static();
      }
    } else {
      socow_vector tmp(cow_array::init(std::as_const(*this).data(), size_, new_capacity), size_);
      *this = tmp;
    }
  }

  void dynamic_to_static() {
    assert(size_ <= SMALL_SIZE);
    cow_array* tmp = dynamic_array_;
    try {
      std::uninitialized_copy_n(tmp->const_data(), size_, static_array_);
    } catch (...) {
      dynamic_array_ = tmp;
      throw;
    }
    cow_array::delete_ptr(tmp, size_);
    is_dynamic_ = false;
  }

  static void swap(socow_vector* first, socow_vector* second) {
    if (first == second) {
      return;
    }
    if (first->is_dynamic_ && second->is_dynamic_) {
      std::swap(first->dynamic_array_, second->dynamic_array_);
      std::swap(first->size_, second->size_);
    } else if (first->is_dynamic_ || second->is_dynamic_) {
      if (second->is_dynamic_) {
        std::swap(first, second);
      }
      socow_vector tmp(*first);
      *first = *second;
      *second = tmp;
    } else {
      if (first->size_ < second->size_) {
        std::swap(first, second);
      }
      std::uninitialized_copy_n(first->begin() + second->size_, first->size() - second->size(), second->end());
      try {
        std::swap_ranges(second->begin(), second->begin() + second->size(), first->begin());
      } catch (...) {
        std::destroy_n(second->begin() + second->size(), first->size() - second->size());
        throw;
      }
      std::destroy_n(first->begin() + second->size(), first->size() - second->size());
      std::swap(first->size_, second->size_);
    }
  }

  struct cow_array {
  public:
    static void delete_ptr(cow_array*& ptr, size_t size) {
      if (ptr->dec_counter()) {
        ptr->clear(size);
        operator delete(ptr);
      }
      ptr = nullptr;
    }

    static cow_array* init(const_pointer array, size_t size, size_t capacity) {
      assert(capacity > 0);
      cow_array* result = static_cast<cow_array*>(operator new(sizeof(cow_array) + capacity * sizeof(value_type)));
      result->capacity_ = capacity;
      result->ptr_counter_ = 1;
      try {
        result->fill(array, size);
      } catch (...) {
        operator delete(result);
        throw;
      }
      return result;
    }

    static cow_array* init(const cow_array& other, size_t size) {
      return init(other.const_data(), size, other.capacity());
    }

    static cow_array* init(const_pointer other, size_t size, size_t capacity, const_reference value, size_t pos) {
      assert(capacity >= size + 1);
      cow_array* result = init(other, pos, capacity);
      try {
        result->push_back(value, pos);
        pos++;
        std::uninitialized_copy_n(other + pos - 1, size - pos + 1, result->data_ + pos);
      } catch (...) {
        result->clear(pos);
        operator delete(result);
        throw;
      }
      return result;
    }

    void inc_counter() noexcept {
      ptr_counter_++;
    }

    bool dec_counter() noexcept {
      return --ptr_counter_ == 0;
    }

    size_t ptr_counter() const noexcept {
      return ptr_counter_;
    }

    size_t capacity() const noexcept {
      return capacity_;
    }

    pointer data() {
      assert(ptr_counter_ == 1);
      return data_;
    }

    const_pointer const_data() const noexcept {
      return data_;
    }

    void push_back(const_reference value, size_t size) {
      assert(ptr_counter_ == 1 && size < capacity_);
      new (data_ + size) value_type(value);
      size++;
    }

    void pop_back(size_t size) {
      assert(ptr_counter_ <= 1 && size > 0);
      data_[size - 1].~value_type();
    }

    void clear(size_t size) {
      assert(ptr_counter_ <= 1 && size <= capacity_);
      std::destroy_n(data_, size);
    }

  private:
    void fill(const_pointer array, size_t size) {
      assert(size <= capacity_);
      std::uninitialized_copy_n(array, size, data_);
    }

    size_t ptr_counter_ = 1;
    size_t capacity_ = 0;
    value_type data_[0];
  };

  socow_vector(cow_array* cow, size_t size) noexcept : dynamic_array_(cow), size_(size), is_dynamic_(true) {}

  union {
    value_type static_array_[SMALL_SIZE];
    cow_array* dynamic_array_ = nullptr;
  };

  size_t size_ = 0;
  bool is_dynamic_ = false;
};
