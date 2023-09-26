#pragma once

#include <array>
#include <cassert>
#include <cstring>
#include <iostream>
#include <vector>

template <size_t N>
class generator {
public:
  generator() noexcept = default;

  generator(char (*const generate)(std::array<uint8_t, N>&), const std::array<uint8_t, N>& start_status) noexcept
      : generate_(generate),
        key_(start_status) {}

  char next_char() noexcept {
    return generate_(key_);
  }

  const std::array<uint8_t, N>& get_status() const noexcept {
    return key_;
  }

  void set_status(const std::array<uint8_t, N>& new_status) noexcept {
    key_ = new_status;
  }

private:
  char (*generate_)(std::array<uint8_t, N>&) = [](std::array<uint8_t, N>&) { return '\0'; };

  std::array<uint8_t, N> key_;
};

template <size_t N>
class generated_istreambuf : public std::basic_streambuf<char> {
public:
  generated_istreambuf(const generator<N>& generator, size_t size) noexcept
      : generator_(generator),
        size_(size),
        start_position_(generator.get_status()) {}

  generated_istreambuf(const generator<N>& generator) noexcept : generated_istreambuf(generator, -1) {}

  ~generated_istreambuf() override = default;

  bool equal(const std::vector<char>& other) const {
    if (size_ != other.size()) {
      return false;
    }
    generator gen_copy = generator_;
    gen_copy.set_status(start_position_);
    for (size_t i = 0; i < size_; i++) {
      char oth = gen_copy.next_char();
      if (other[i] != oth) {
        return false;
      }
    }
    return true;
  }

protected:
  void imbue(const std::locale& loc) override {
    return;
  }

  basic_streambuf* setbuf(char_type* s, std::streamsize n) override {
    return static_cast<basic_streambuf*>(this);
  }

  pos_type seekoff(off_type off, std::ios_base::seekdir dir,
                   std::ios_base::openmode which = std::ios_base::in | std::ios_base::out) override {
    assert(which & std::ios_base::in);
    if (dir == std::ios_base::end) {
      off += size_;
    } else if (dir == std::ios_base::cur) {
      off += now_position_;
    }
    assert(off >= 0 && off <= size_);
    return seekpos(off);
  }

  pos_type seekpos(pos_type pos, std::ios_base::openmode which = std::ios_base::in | std::ios_base::out) override {
    assert(which & std::ios_base::in);
    if (pos < now_position_) {
      generator_.set_status(start_position_);
      now_position_ = 0;
    }
    while (now_position_ < pos) {
      generator_.next_char();
      now_position_++;
    }
    return now_position_;
  }

  std::streamsize showmanyc() override {
    return size_ - now_position_;
  }

  int_type underflow() override {
    if (now_position_ < size_) {
      now_position_++;
      return generator_.next_char();
    } else {
      return traits_type::eof();
    }
  }

  int_type uflow() override {
    if (now_position_ < size_) {
      generator gen_copy = generator_;
      return gen_copy.next_char();
    } else {
      return traits_type::eof();
    }
  }

  std::streamsize xsgetn(char_type* s, std::streamsize count) override {
    std::streamsize readed_chars = 0;
    while (readed_chars < count && now_position_ < size_) {
      s[readed_chars++] = generator_.next_char();
      now_position_++;
    }
    return readed_chars;
  }

  int_type overflow(int_type ch = traits_type::eof()) override {
    return traits_type::eof();
  }

  std::streamsize xsputn(const char_type* s, std::streamsize count) override {
    return 0;
  }

  int_type pbackfail(int_type c = traits_type::eof()) override {
    return traits_type::eof();
  }

private:
  generator<N> generator_;
  const std::array<uint8_t, N> start_position_;
  size_t now_position_ = 0;
  size_t size_;
};

class vector_streambuf : public std::basic_streambuf<char> {
public:
  std::vector<char>& buffer;

  vector_streambuf(std::vector<char>& buffer_) : pos_(0), buffer(buffer_) {}

  void seekg(size_t pos) {
    pos_ = pos;
  }

  pos_type seekpos(pos_type pos, std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) override {
    return seekoff(pos - pos_type(off_type(0)), std::ios_base::beg, mode);
  }

  pos_type seekoff(off_type off, std::ios_base::seekdir dir,
                   std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) override {
    switch (dir) {
    case std::ios_base::beg:
      pos_ = off;
      break;
    case std::ios_base::cur:
      pos_ += off;
      break;
    case std::ios_base::end:
      pos_ = buffer.size() + off;
      break;
    default:
      break;
    }
    return pos_;
  }

  int_type overflow(int_type c) override {
    if (c != EOF) {
      buffer.push_back(static_cast<char>(c));
    }
    return c;
  }

  std::streamsize xsputn(const char* s, std::streamsize count) override {
    buffer.insert(buffer.end(), s, s + count);
    return count;
  }

  int_type underflow() override {
    if (pos_ < buffer.size()) {
      return buffer[pos_];
    } else {
      return traits_type::eof();
    }
  }

  int_type uflow() override {
    if (pos_ < buffer.size()) {
      return static_cast<unsigned char>(buffer[pos_++]);
    } else {
      return EOF;
    }
  }

  int_type pbackfail(int_type c) override {
    const auto prev = pos_ - 1;
    if (c != traits_type::eof() && prev < buffer.size() && c != buffer[prev]) {
      return traits_type::eof();
    }

    pos_ = prev;
    return 1;
  }

  std::streamsize xsgetn(char* s, std::streamsize n) override {
    if (pos_ < buffer.size()) {
      const size_t count = std::min<size_t>(n, buffer.size() - pos_);
      std::memcpy(s, &buffer[pos_], count);
      pos_ += count;
      return count;
    }
    return 0;
  }

private:
  size_t pos_ = 0;
};
