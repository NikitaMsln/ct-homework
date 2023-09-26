#include "hided_iostream_work.h"

#include <cassert>

bit_sequence::bit_sequence() noexcept = default;

bit_sequence::bit_sequence(size_t n) : data_((n + 7) / 8, 0), offset_((n % 8 == 0) ? 0 : 8 - n % 8) {}

size_t bit_sequence::size() const noexcept {
  assert(offset_ <= 7);
  return data_.size() * 8 - offset_;
}

void bit_sequence::push_bit(bool value) {
  assert(offset_ <= 7);
  if (offset_ == 0) {
    data_.push_back(value << 7);
    offset_ = 7;
  } else {
    data_.back() += (value & 1) << (--offset_);
  }
}

void bit_sequence::push_byte(uint8_t value) {
  if (offset_ > 0) {
    data_.back() += value >> (8 - offset_);
  }
  data_.push_back(value << offset_);
}

std::byte bit_sequence::cut(size_t begin, size_t end) const noexcept {
  assert(offset_ <= 7);
  assert(begin <= end);
  assert(begin <= size() && end <= size());
  if (begin == end) {
    return static_cast<std::byte>(0);
  }
  if (begin / 8 == (end - 1) / 8) {
    return static_cast<std::byte>((data_[begin / 8] << (begin % 8)) >> (begin % 8 + 7 - (end - 1) % 8));
  } else {
    return static_cast<std::byte>((((data_[begin / 8] << (begin % 8)) >> (begin % 8)) << (end % 8)) +
                                  (data_[(end - 1) / 8] >> (7 - (end - 1) % 8)));
  }
}

bit_sequence& bit_sequence::operator++() {
  assert(size() > 0);
  size_t now_pos = data_.size();
  uint8_t overflow = 1 << offset_;
  while (now_pos > 0 && overflow > 0) {
    if (data_[now_pos - 1] > UINT8_MAX - overflow) {
      data_[now_pos - 1] = 0;
      overflow = 1;
    } else {
      data_[now_pos - 1] += overflow;
      overflow = 0;
    }
    now_pos--;
  }
  assert(overflow == 0);
  return *this;
}

bit_sequence bit_sequence::operator++(int) {
  bit_sequence tmp = *this;
  ++*this;
  return tmp;
}
