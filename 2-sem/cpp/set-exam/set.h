#pragma once

#include <cassert>
#include <iterator>
#include <utility>

template <typename T>
class set {
public:
  using value_type = T;

  using reference = T&;
  using const_reference = const T&;

  using pointer = T*;
  using const_pointer = const T*;

private:
  struct empty_node {
    empty_node* left = nullptr;
    empty_node* parent = nullptr;
    empty_node* right = nullptr;
    empty_node& operator=(empty_node&) = delete;
  };

  struct node : empty_node {
    const value_type value;

    node(empty_node* parent_node, const_reference value) : value(value) {
      this->parent = parent_node;
    }

    node& operator=(node&) = delete;
  };

  static node* left_node(empty_node* element) {
    return static_cast<node*>(element->left);
  }

  static node* right_node(empty_node* element) {
    return static_cast<node*>(element->right);
  }

  static const node* left_node(const empty_node* element) {
    return static_cast<node*>(element->left);
  }

  static const node* right_node(const empty_node* element) {
    return static_cast<node*>(element->right);
  }

  struct basic_iterator {
  public:
    friend set;

    using value_type = T;

    using reference = const_reference;

    using pointer = const_pointer;

    using difference_type = std::ptrdiff_t;
    using iterator_category = std::bidirectional_iterator_tag;

    basic_iterator() noexcept = default;

    basic_iterator(const empty_node& value) noexcept : data_(&value) {}

    basic_iterator& operator++() {
      if (data_->right != nullptr) {
        data_ = data_->right;
        while (data_->left != nullptr) {
          data_ = data_->left;
        }
      } else {
        while (data_->parent->left != data_) {
          data_ = data_->parent;
        }
        data_ = data_->parent;
      }
      return *this;
    }

    basic_iterator& operator--() {
      if (data_->left != nullptr) {
        data_ = data_->left;
        while (data_->right != nullptr) {
          data_ = data_->right;
        }
      } else {
        while (data_->parent->left == data_) {
          data_ = data_->parent;
        }
        data_ = data_->parent;
      }
      return *this;
    }

    basic_iterator operator++(int) {
      basic_iterator temp = *this;
      ++*this;
      return temp;
    }

    basic_iterator operator--(int) {
      basic_iterator temp = *this;
      --*this;
      return temp;
    }

    reference operator*() const {
      return static_cast<const node*>(data_)->value;
    }

    pointer operator->() const {
      return &operator*();
    }

    bool operator==(const basic_iterator& other) const noexcept {
      return data_ == other.data_;
    }

    bool operator!=(const basic_iterator& other) const noexcept {
      return data_ != other.data_;
    }

    ~basic_iterator() noexcept = default;

    friend void swap(basic_iterator& left, basic_iterator& right) {
      std::swap(left.data_, right.data_);
    }

  private:
    const empty_node* data_ = nullptr;
  };

public:
  using iterator = basic_iterator;
  using const_iterator = basic_iterator;

  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

public:
  // O(1) nothrow
  set() noexcept = default;

  // O(n) strong
  set(const set& other) {
    if (other.empty()) {
      return;
    }
    start_.left = copy(left_node(&other.start_), &start_);
    size_ = other.size_;
  }

  // O(n) strong
  set& operator=(const set& other) {
    if (this == &other) {
      return *this;
    }
    set tmp = other;
    swap(*this, tmp);
    return *this;
  }

  // O(n) nothrow
  ~set() noexcept {
    clear();
  }

  // O(n) nothrow
  void clear() noexcept {
    if (!empty()) {
      clear(left_node(&start_));
      start_.left = nullptr;
    }
    size_ = 0;
  }

  // O(1) nothrow
  size_t size() const noexcept {
    return size_;
  }

  // O(1) nothrow
  bool empty() const noexcept {
    return size_ == 0;
  }

  // nothrow
  const_iterator begin() const noexcept {
    const empty_node* result = &start_;
    while (result->left != nullptr) {
      result = result->left;
    }
    return basic_iterator(*result);
  }

  // nothrow
  const_iterator end() const noexcept {
    return basic_iterator(start_);
  }

  // nothrow
  const_reverse_iterator rbegin() const noexcept {
    return reverse_iterator(end());
  }

  // nothrow
  const_reverse_iterator rend() const noexcept {
    return reverse_iterator(begin());
  }

  // O(h) strong
  std::pair<iterator, bool> insert(const_reference value) {
    if (empty()) {
      start_.left = new node(&start_, value);
      size_++;
      return std::pair(basic_iterator(*start_.left), true);
    }
    node* element = left_node(&start_);
    while (true) {
      if (element->value == value) {
        return std::pair(basic_iterator(*element), false);
      } else if (element->value > value) {
        if (element->left == nullptr) {
          element->left = new node(element, value);
          size_++;
          return std::pair(basic_iterator(*element->left), true);
        }
        element = left_node(element);
      } else {
        if (element->right == nullptr) {
          element->right = new node(element, value);
          size_++;
          return std::pair(basic_iterator(*element->right), true);
        }
        element = right_node(element);
      }
    }
  }

  // O(h) nothrow
  iterator erase(const_iterator pos) {
    empty_node* element = const_cast<empty_node*>(pos.data_);
    if (element == &start_) {
      return end();
    }

    ++pos;

    bool is_left = element->parent->left == element;
    if (element->left == nullptr && element->right == nullptr) {
      if (is_left) {
        element->parent->left = nullptr;
      } else {
        element->parent->right = nullptr;
      }
    } else if (element->left == nullptr && element->right != nullptr) {
      bind(element->parent, right_node(element), is_left);
    } else if (element->left != nullptr && element->right == nullptr) {
      bind(element->parent, left_node(element), is_left);
    } else {
      node* new_node = right_node(element);
      while (new_node->left != nullptr) {
        new_node = left_node(new_node);
      }
      if (new_node->right != nullptr) {
        bind(new_node->parent, right_node(new_node), new_node->parent->left == new_node);
      } else if (new_node->parent->left == new_node) {
        new_node->parent->left = nullptr;
      } else {
        new_node->parent->right = nullptr;
      }
      bind(element->parent, new_node, is_left);
      bind(new_node, left_node(element), true);
      if (element->right != nullptr) {
        bind(new_node, right_node(element), false);
      } else {
        new_node->right = nullptr;
      }
    }
    delete static_cast<node*>(element);
    size_--;
    return pos;
  }

  // O(h) strong
  size_t erase(const_reference value) {
    const_iterator deleted = find(value);
    if (deleted == end()) {
      return 0;
    } else {
      erase(deleted);
      return 1;
    }
  }

  // O(h) strong
  const_iterator lower_bound(const_reference value) const {
    const empty_node* result = find_lower_bound(left_node(&start_), value);
    if (result == nullptr) {
      return end();
    } else {
      return basic_iterator(*result);
    }
  }

  // O(h) strong
  const_iterator upper_bound(const_reference value) const {
    const_iterator result = lower_bound(value);
    if (result != end()) {
      if (*result == value) {
        result++;
      }
      return result;
    } else {
      return end();
    }
  }

  // O(h) strong
  const_iterator find(const_reference value) const {
    const_iterator result = lower_bound(value);
    if (result != end() && *result == value) {
      return result;
    } else {
      return end();
    }
  }

  // O(1) nothrow
  friend void swap(set& left, set& right) noexcept {
    std::swap(left.start_.left, right.start_.left);
    if (left.start_.left != nullptr) {
      left.start_.left->parent = &left.start_;
    }
    if (right.start_.left != nullptr) {
      right.start_.left->parent = &right.start_;
    }
    std::swap(left.size_, right.size_);
  }

private:
  static node* copy(const node* in, empty_node* parent) {
    if (in == nullptr) {
      return nullptr;
    }

    node* result = new node(parent, in->value);

    try {
      result->left = copy(left_node(in), result);
      result->right = copy(right_node(in), result);
    } catch (...) {
      clear(result);
      throw;
    }
    return result;
  }

  static const node* find_lower_bound(const node* tree, const_reference value) {
    if (tree == nullptr || tree->value == value) {
      return tree;
    }
    if (value <= tree->value) {
      const node* result = find_lower_bound(left_node(tree), value);
      if (result == nullptr) {
        return tree;
      } else {
        return result;
      }
    } else {
      return find_lower_bound(right_node(tree), value);
    }
  }

  static void bind(empty_node* parent, node* child, bool is_left) {
    if (is_left) {
      parent->left = child;
    } else {
      parent->right = child;
    }
    child->parent = parent;
  }

  static void clear(node* element) noexcept {
    if (element == nullptr) {
      return;
    }
    clear(left_node(element));
    clear(right_node(element));
    delete element;
  }

  empty_node start_;
  size_t size_ = 0;
};
