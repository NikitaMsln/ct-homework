#include "hided_iostream_work.h"

#include <iostream>

buffered_ostream::buffered_ostream(std::ostream& out) noexcept : out_(out) {}

void buffered_ostream::write_byte(std::byte byte) {
  if (buffer_position_ == BUFFER_SIZE) {
    flush();
  }
  buffer_[buffer_position_++] = byte;
}

void buffered_ostream::flush() {
  out_.write(reinterpret_cast<char*>(buffer_), static_cast<std::streamsize>(buffer_position_));
  buffer_position_ = 0;
}
