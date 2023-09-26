#include "hided_iostream_work.h"

#include <iostream>

buffered_istream::buffered_istream(std::istream& in) noexcept : in_(in) {}

buffered_istream::buffered_istream(buffered_istream& in) noexcept
    : in_(in.in_),
      buffer_position_(in.buffer_position_),
      buffer_capacity_(in.buffer_capacity_) {
  for (size_t i = buffer_position_; i < buffer_capacity_; i++) {
    buffer_[i] = in.buffer_[i];
  }
  in.~buffered_istream();
}

bool buffered_istream::is_ended() noexcept {
  if (buffer_position_ >= buffer_capacity_) {
    fill_buffer();
  }
  return buffer_position_ >= buffer_capacity_;
}

std::byte buffered_istream::next_byte() {
  if (buffer_position_ >= buffer_capacity_) {
    fill_buffer();
    if (buffer_capacity_ == 0) {
      throw std::ios_base::failure("Reading failed");
    }
  }
  return buffer_[buffer_position_++];
}

void buffered_istream::fill_buffer() {
  in_.read(reinterpret_cast<char*>(buffer_), BUFFER_SIZE);
  buffer_capacity_ = in_.gcount();
  buffer_position_ = 0;
}
