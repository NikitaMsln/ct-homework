#pragma once
#include <array>
#include <compare>
#include <iostream>
#include <map>
#include <vector>

class buffered_istream {
public:
  static const size_t BUFFER_SIZE = 4096;
  explicit buffered_istream(std::istream&) noexcept;
  buffered_istream(buffered_istream&) noexcept;
  buffered_istream(const buffered_istream&) = delete;
  buffered_istream& operator=(const buffered_istream&) = delete;
  std::byte next_byte();
  bool is_ended() noexcept;

private:
  void fill_buffer();
  std::istream& in_;
  std::byte buffer_[BUFFER_SIZE];
  size_t buffer_position_ = 0;
  size_t buffer_capacity_ = 0;
};

class buffered_ostream {
public:
  static const size_t BUFFER_SIZE = 4096;
  explicit buffered_ostream(std::ostream&) noexcept;
  buffered_ostream(const buffered_ostream&) = delete;
  buffered_ostream& operator=(const buffered_ostream&) = delete;
  void write_byte(std::byte);
  void flush();

private:
  std::ostream& out_;
  std::byte buffer_[BUFFER_SIZE];
  size_t buffer_position_ = 0;
};

class bit_sequence {
public:
  bit_sequence() noexcept;
  explicit bit_sequence(size_t);
  size_t size() const noexcept;
  void push_bit(bool);
  void push_byte(uint8_t);
  std::byte cut(size_t, size_t) const noexcept;
  bit_sequence& operator++();
  bit_sequence operator++(int);

private:
  size_t offset_ = 0;
  std::vector<uint8_t> data_ = {};
};

class bit_reader {
public:
  explicit bit_reader(std::istream&) noexcept;
  explicit bit_reader(buffered_istream&) noexcept;
  bit_reader(const bit_reader&) = delete;
  bit_reader& operator=(const bit_reader&) = delete;
  bool get_bit();
  bit_sequence get_bit_sequence(size_t);
  bool is_ended();

private:
  size_t offset_ = 0;
  std::byte value_;
  buffered_istream in_;
};

class bit_writer {
public:
  explicit bit_writer(std::ostream&) noexcept;
  bit_writer(const bit_writer&) = delete;
  bit_writer& operator=(const bit_writer&) = delete;
  void write_bit_sequence(const bit_sequence&);
  void local_flush();
  void flush();

private:
  size_t offset_ = 8;
  std::byte value_ = static_cast<std::byte>(0);
  buffered_ostream out_;
};

class huffman_tree {
private:
  struct node {
    node* left = nullptr;
    node* right = nullptr;
    char value;
    bool is_leaf = true;
  };

public:
  class iterator {
    friend huffman_tree;

  public:
    bool is_leaf() const;
    char get_value() const;
    void next(bool);

  private:
    iterator(const node*) noexcept;
    const node* now_pos_;
  };

  static const size_t SIZE = 256;

public:
  explicit huffman_tree(const std::array<size_t, SIZE>& hist);

  explicit huffman_tree(const bit_sequence& serialized_tree);

  huffman_tree(const huffman_tree&) = delete;

  huffman_tree& operator=(const huffman_tree&) = delete;

  ~huffman_tree() noexcept;

  iterator begin() const;

  std::array<bit_sequence, SIZE> get_code() const;

  bit_sequence serialize() const noexcept;

private:
  static void make_code(const node*, std::array<bit_sequence, SIZE>&, const bit_sequence&);
  static void serialize(const node*, bit_sequence&);
  static node* deserialize(const bit_sequence&, size_t&, size_t&);
  static void clear(node*) noexcept;
  node core_;
  size_t count_;
};
