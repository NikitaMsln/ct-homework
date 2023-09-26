#include "hided_iostream_work.h"

#include <set>

bool huffman_tree::iterator::is_leaf() const {
  return now_pos_->is_leaf;
}

char huffman_tree::iterator::get_value() const {
  return now_pos_->value;
}

void huffman_tree::iterator::next(bool child) {
  now_pos_ = child ? now_pos_->right : now_pos_->left;
}

huffman_tree::iterator::iterator(const node* pos) noexcept : now_pos_(pos) {}

huffman_tree::huffman_tree(const std::array<size_t, SIZE>& hist) : count_(0) {
  auto nodes_comparator = [](const std::pair<node*, size_t>& x, const std::pair<node*, size_t>& y) {
    return x.second < y.second || (x.second == y.second && x.first < y.first);
  };
  std::set<std::pair<node*, size_t>, decltype(nodes_comparator)> nodes;

  for (size_t i = 0; i < SIZE; i++) {
    if (hist[i] > 0) {
      count_++;
      node* leaf = new node;
      leaf->is_leaf = true;
      leaf->value = static_cast<char>(i);
      nodes.insert(std::pair<node*, size_t>(leaf, hist[i]));
    }
  }

  if (count_ == 0) {
    return;
  }

  while (nodes.size() > 1) {
    auto last = nodes.begin();
    auto first = last++;
    auto left = *first;
    auto right = *last;

    nodes.erase(first);
    nodes.erase(last);

    node* new_node = new node;
    new_node->is_leaf = false;
    new_node->left = left.first;
    new_node->right = right.first;
    left.first = new_node;
    left.second += right.second;

    nodes.insert(left);
  }

  core_ = *(nodes.begin()->first);
  delete nodes.begin()->first;
}

huffman_tree::huffman_tree(const bit_sequence& serialized_tree) : count_(0) {
  size_t pos = 0;
  node* core_copy = deserialize(serialized_tree, pos, count_);
  core_ = *core_copy;
  delete core_copy;
}

huffman_tree::node* huffman_tree::deserialize(const bit_sequence& serialized, size_t& pos, size_t& count) {
  bool is_leaf = (static_cast<uint8_t>(serialized.cut(pos, pos + 1)) & 1);
  pos++;
  node* result = new node;
  result->is_leaf = is_leaf;
  if (is_leaf) {
    result->value = static_cast<char>(serialized.cut(pos, pos + 8));
    pos += 8;
    count++;
  } else {
    result->left = deserialize(serialized, pos, count);
    result->right = deserialize(serialized, pos, count);
  }
  return result;
}

huffman_tree::~huffman_tree() noexcept {
  clear(core_.left);
  clear(core_.right);
}

void huffman_tree::clear(node* now_node) noexcept {
  if (now_node == nullptr) {
    return;
  }
  clear(now_node->left);
  clear(now_node->right);
  delete now_node;
}

huffman_tree::iterator huffman_tree::begin() const {
  return iterator(&core_);
}

std::array<bit_sequence, huffman_tree::SIZE> huffman_tree::get_code() const {
  std::array<bit_sequence, SIZE> result;
  make_code(&core_, result, bit_sequence());
  return result;
}

void huffman_tree::make_code(const node* now_node, std::array<bit_sequence, SIZE>& result,
                             const bit_sequence& sequence) {
  if (now_node->is_leaf) {
    if (sequence.size() > 0) {
      result[static_cast<uint8_t>(now_node->value)] = sequence;
    }
    return;
  }

  auto left_sequence = sequence;
  left_sequence.push_bit(false);
  make_code(now_node->left, result, left_sequence);

  auto right_sequence = sequence;
  right_sequence.push_bit(true);
  make_code(now_node->right, result, right_sequence);
}

bit_sequence huffman_tree::serialize() const noexcept {
  bit_sequence result;
  if (count_ > 0) {
    serialize(&core_, result);
  }
  return result;
}

void huffman_tree::serialize(const node* now_node, bit_sequence& result) {
  if (now_node->is_leaf) {
    result.push_bit(true);
    result.push_byte(now_node->value);
    return;
  } else {
    result.push_bit(false);
    serialize(now_node->left, result);
    serialize(now_node->right, result);
  }
}
