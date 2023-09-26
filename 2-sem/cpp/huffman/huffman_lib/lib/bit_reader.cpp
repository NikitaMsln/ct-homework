#include "hided_iostream_work.h"

#include <cassert>
#include <iostream>

bit_reader::bit_reader(std::istream& in) noexcept : in_(in), value_(static_cast<std::byte>(0)) {}

bit_reader::bit_reader(buffered_istream& in) noexcept : in_(in), value_(static_cast<std::byte>(0)) {}

bool bit_reader::get_bit() {
  assert(offset_ <= 8);
  if (offset_ == 0) {
    value_ = in_.next_byte();
    offset_ = 8;
  }
  return static_cast<uint8_t>(value_) & (1 << (--offset_));
}

bit_sequence bit_reader::get_bit_sequence(size_t count) {
  assert(offset_ <= 8);
  size_t readed_bits = 0;
  bit_sequence result;
  for (; readed_bits < count && offset_ > 0; readed_bits++) {
    result.push_bit(get_bit());
  }
  while (count - readed_bits >= 8) {
    result.push_byte(static_cast<uint8_t>(in_.next_byte()));
    readed_bits += 8;
  }
  for (; readed_bits < count; readed_bits++) {
    result.push_bit(get_bit());
  }
  return result;
}

bool bit_reader::is_ended() {
  return in_.is_ended() && offset_ == 0;
}
