#include "hided_iostream_work.h"

#include <iostream>

bit_writer::bit_writer(std::ostream& out) noexcept : out_(out) {}

void bit_writer::write_bit_sequence(const bit_sequence& value) {
  size_t written_bits_count = 0;
  while (written_bits_count < value.size()) {
    size_t now_written_bits_count = std::min(value.size() - written_bits_count, offset_);
    value_ = static_cast<std::byte>(
        static_cast<uint8_t>(value_) |
        (static_cast<uint8_t>(value.cut(written_bits_count, written_bits_count + now_written_bits_count))
         << (offset_ - now_written_bits_count)));
    written_bits_count += now_written_bits_count;
    offset_ -= now_written_bits_count;
    if (offset_ == 0) {
      local_flush();
    }
  }
}

void bit_writer::local_flush() {
  out_.write_byte(value_);
  value_ = static_cast<std::byte>(0);
  offset_ = 8;
}

void bit_writer::flush() {
  local_flush();
  out_.flush();
}
