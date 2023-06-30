#pragma once

#include <cassert>
#include <cstddef>
#include <iterator>
#include <utility>

template <typename T>
class list {
public:
  using value_type = T;

  using reference = T&;
  using const_reference = const T&;

  using pointer = T*;
  using const_pointer = const T*;

private:
  struct empty_node {
    empty_node* next = this;
    empty_node* last = this;
  };

  struct node : empty_node {
    value_type value;

    node(const value_type value) : value(value) {}
  };

  template <typename U>
  struct basic_iterator {
    friend list<T>;

    using value_type = T;

    using reference = U&;

    using pointer = U*;

    using difference_type = std::ptrdiff_t;
    using iterator_category = std::bidirectional_iterator_tag;

    basic_iterator() = default;

    basic_iterator& operator++() noexcept {
      data = data->next;
      return *this;
    }

    basic_iterator& operator--() noexcept {
      data = data->last;
      return *this;
    }

    basic_iterator operator++(int) noexcept {
      basic_iterator temp = *this;
      ++*this;
      return temp;
    }

    basic_iterator operator--(int) noexcept {
      basic_iterator temp = *this;
      --*this;
      return temp;
    }

    reference operator*() const {
      return static_cast<node*>(data)->value;
    }

    pointer operator->() const {
      return &operator*();
    }

    bool operator==(const basic_iterator& other) const noexcept {
      return data == other.data;
    }

    bool operator!=(const basic_iterator& other) const noexcept {
      return data != other.data;
    }

    template <typename P>
    basic_iterator& operator=(const basic_iterator<P>& other) {
      data = other.data;
    }

    operator basic_iterator<const U>() const {
      return basic_iterator<const U>(data);
    }

    ~basic_iterator() = default;

    friend void swap(basic_iterator& left, basic_iterator& right) {
      std::swap(left.data, right.data);
    }

  private:
    empty_node* data;

    explicit basic_iterator(const empty_node* data) : data(const_cast<empty_node*>(data)) {}

    friend basic_iterator operator+(const basic_iterator& it, difference_type diff) {
      empty_node* res = it.data;
      while (diff--) {
        res = res->next;
      }
      return basic_iterator(res);
    }

    friend basic_iterator operator+(difference_type diff, const basic_iterator& it) {
      return it + diff;
    }

    friend basic_iterator operator-(const basic_iterator& it, difference_type diff) {
      empty_node* res = it.data;
      while (diff--) {
        res = res->last;
      }
      return basic_iterator(res);
    }
  };

public:
  using iterator = basic_iterator<value_type>;
  using const_iterator = basic_iterator<const value_type>;

  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

public:
  // O(1), nothrow
  list() noexcept = default;

  // O(n), strong
  list(const list& other) : list(other.begin(), other.end()) {}

  // O(n), strong
  template <std::input_iterator InputIt>
  list(InputIt first, InputIt last) {
    list tmp;
    for (InputIt i = first; i != last; i++) {
      tmp.push_back(*i);
    }
    swap(*this, tmp);
  }

  // O(n), strong
  list& operator=(const list& other) {
    if (this == &other) {
      return *this;
    }
    list tmp(other);
    swap(*this, tmp);
    return *this;
  }

  // O(n), nothrow
  ~list() noexcept {
    clear();
  }

  // O(1), nothrow
  bool empty() const noexcept {
    return size_ == 0;
  }

  // O(1), nothrow
  size_t size() const noexcept {
    return size_;
  }

  // O(1), nothrow
  T& front() {
    assert(size_ > 0);
    return *begin();
  }

  // O(1), nothrow
  const T& front() const {
    assert(size_ > 0);
    return *begin();
  }

  // O(1), nothrow
  T& back() {
    assert(size_ > 0);
    return *(end() - 1);
  }

  // O(1), nothrow
  const T& back() const {
    assert(size_ > 0);
    return *(end() - 1);
  }

  // O(1), strong
  void push_front(const T& value) {
    insert(begin(), value);
  }

  // O(1), strong
  void push_back(const T& value) {
    insert(end(), value);
  }

  // O(1), nothrow
  void pop_front() {
    assert(size_ > 0);
    erase(begin());
  }

  // O(1), nothrow
  void pop_back() {
    assert(size_ > 0);
    erase(end() - 1);
  }

  // O(1), nothrow
  iterator begin() noexcept {
    return iterator(bound_.next);
  }

  // O(1), nothrow
  const_iterator begin() const noexcept {
    return const_iterator(bound_.next);
  }

  // O(1), nothrow
  iterator end() noexcept {
    return iterator(&bound_);
  }

  // O(1), nothrow
  const_iterator end() const noexcept {
    return const_iterator(&bound_);
  }

  // O(1), nothrow
  reverse_iterator rbegin() noexcept {
    return reverse_iterator(iterator(&bound_));
  }

  // O(1), nothrow
  const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(const_iterator(&bound_));
  }

  // O(1), nothrow
  reverse_iterator rend() noexcept {
    return reverse_iterator(iterator(bound_.next));
  }

  // O(1), nothrow
  const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(const_iterator(bound_.next));
  }

  // O(n), nothrow
  void clear() noexcept {
    erase(begin(), end());
  }

  // O(1), strong
  iterator insert(const_iterator pos, const T& value) {
    node* new_element = new node(value);
    bind(pos.data->last, new_element);
    bind(new_element, pos.data);
    size_++;
    return iterator(new_element);
  }

  // O(last - first), strong
  template <std::input_iterator InputIt>
  iterator insert(const_iterator pos, InputIt first, InputIt last) {
    empty_node* before = pos.data->last;
    list tmp(first, last);
    splice(pos, tmp, tmp.begin(), tmp.end());
    return iterator(before->next);
  }

  // O(1), nothrow
  iterator erase(const_iterator pos) noexcept {
    return erase(pos, pos + 1);
  }

  // O(last - first), nothrow
  iterator erase(const_iterator first, const_iterator last) noexcept {
    empty_node* before = first.data->last;
    empty_node* first_data = first.data;
    empty_node* last_data = last.data;
    while (first_data != last_data) {
      empty_node* removed_element = first_data;
      first_data = first_data->next;
      delete (static_cast<node*>(removed_element));
      size_--;
    }
    bind(before, last_data);
    return iterator(before->next);
  }

  // O(last - first) in general but O(1) when possible, nothrow
  void splice(const_iterator pos, list& other, const_iterator first, const_iterator last) noexcept {
    if (first == last) {
      return;
    }
    if (&other != this) {
      size_t diff =
          (&other.bound_ != last.data || &other.bound_ != first.data->last) ? std::distance(first, last) : other.size_;
      size_ += diff;
      other.size_ -= diff;
    }
    empty_node* before = pos.data->last;
    empty_node* other_before = first.data->last;
    empty_node* slice_last = last.data->last;
    bind(before, first.data);
    bind(slice_last, pos.data);
    bind(other_before, last.data);
  }

  // O(1), nothrow
  friend void swap(list& left, list& right) noexcept {
    if (&left == &right) {
      return;
    }
    empty_node* left_t = left.bound_.next;
    bind(&left.bound_, right.bound_.next);
    bind(&right.bound_, left_t);
    left_t = left.bound_.last;
    bind(right.bound_.last, &left.bound_);
    bind(left_t, &right.bound_);
    std::swap(left.size_, right.size_);
  }

private:
  static void bind(empty_node* from, empty_node* to) {
    from->next = to;
    to->last = from;
  }

  empty_node bound_;
  size_t size_ = 0;
};
