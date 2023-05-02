#include "big_integer.h"

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <ostream>
#include <stdexcept>
#include <vector>

big_integer::big_integer() : sign(POSITIVE), big_digits(std::vector<uint32_t>()) {}

big_integer::big_integer(const big_integer& other) : sign(other.sign), big_digits(other.big_digits) {}

big_integer::big_integer(int a) : big_integer(static_cast<long long>(a)) {}

big_integer::big_integer(long a) : big_integer(static_cast<long long>(a)) {}

big_integer::big_integer(long long a) : sign((a >= 0) ? POSITIVE : NEGATIVE), big_digits(std::vector<uint32_t>()) {
  unsigned long long b = (a < 0) ? -static_cast<unsigned long long>(a) : a;
  while (b > 0) {
    big_digits.push_back(b & UINT32_MAX);
    b >>= 32;
  }
  remove_zeros();
}

big_integer::big_integer(unsigned int a) : big_integer(static_cast<unsigned long long>(a)) {}

big_integer::big_integer(unsigned long a) : big_integer(static_cast<unsigned long long>(a)) {}

big_integer::big_integer(unsigned long long a) : sign(POSITIVE), big_digits(std::vector<uint32_t>()) {
  while (a > 0) {
    big_digits.push_back(a & UINT32_MAX);
    a >>= 32;
  }
  remove_zeros();
}

big_integer::big_integer(const std::string& str) : sign(POSITIVE), big_digits(std::vector<uint32_t>()) {
  if (str.length() == 0 || str == "-") {
    throw std::invalid_argument("string in big_integer(std::string) is empty");
  }
  size_t i = 0;
  if (str[i] == '-') {
    sign = NEGATIVE;
    i++;
  }
  for (; i < str.size(); i++) {
    if (str[i] < '0' || str[i] > '9') {
      throw std::invalid_argument("string is unable to parse to big_integer: " + str);
    }
    *this *= RADIX;
    *this += big_integer((str[i] - '0') * ((sign == big_integer::POSITIVE)? 1 : -1));
  }
}

big_integer::~big_integer() = default;

void big_integer::remove_zeros() {
  for (; !big_digits.empty() && big_digits.back() == 0; big_digits.pop_back())
    ;
}

big_integer& big_integer::operator=(const big_integer& other) {
  big_digits = other.big_digits;
  sign = other.sign;
  return *this;
}

void big_integer::add(std::vector<uint32_t>::iterator result, std::vector<uint32_t>::const_iterator other,
                      size_t other_size) const {
  uint64_t overflow = 0;
  size_t digit;
  for (digit = 0; overflow > 0 || digit < other_size; digit++, result++) {
    if (digit < other_size) {
      overflow += static_cast<uint64_t>(*other);
      other++;
    }
    if (digit < big_digits.size()) {
      overflow += static_cast<uint64_t>(big_digits[digit]);
    }
    *result = static_cast<uint32_t>(overflow & UINT32_MAX);
    overflow = overflow >> 32;
  }
}

void big_integer::subtract(std::vector<uint32_t>::iterator result, std::vector<uint32_t>::const_iterator other,
                           size_t other_size) const {
  uint64_t overflow = 1;
  for (size_t digit = 0; digit < big_digits.size(); digit++, result++) {
    overflow = (static_cast<uint64_t>(1) << 32) + overflow - 1;
    if (digit < other_size) {
      overflow -= *other;
      other++;
    }
    overflow += big_digits[digit];
    *result = static_cast<uint32_t>(overflow & UINT32_MAX);
    overflow = overflow >> 32;
  }
}

uint32_t big_integer::divide(uint32_t rhs) {
  if (rhs == 0) {
    throw std::logic_error("division by zero");
  }
  uint64_t overflow = 0;
  for (size_t digit = big_digits.size(); digit > 0; digit--) {
    overflow = big_digits[digit - 1] + (overflow << 32);
    big_digits[digit - 1] = overflow / rhs;
    overflow = overflow % rhs;
  }
  remove_zeros();
  return overflow;
}

big_integer& big_integer::operator+=(const big_integer& rhs) {
  while (rhs.big_digits.size() > big_digits.size()) {
    big_digits.push_back(0);
  }
  if (sign == rhs.sign) {
    big_digits.push_back(0);
    add(big_digits.begin(), rhs.big_digits.begin(), rhs.big_digits.size());
  } else {
    if (abs(*this) >= abs(rhs)) {
      subtract(big_digits.begin(), rhs.big_digits.begin(), rhs.big_digits.size());
    } else {
      sign = !sign;
      rhs.subtract(big_digits.begin(), big_digits.begin(), big_digits.size());
    }
  }
  remove_zeros();
  return *this;
}

big_integer& big_integer::operator-=(const big_integer& rhs) {
  return (*this += -rhs);
}

big_integer& big_integer::operator*=(const big_integer& rhs) {
  sign = (sign == rhs.sign) ? POSITIVE : NEGATIVE;
  std::vector<uint32_t> result(big_digits.size() + rhs.big_digits.size(), 0);

  auto product_result = result.begin();
  for (size_t i = 0; i < big_digits.size(); i++, product_result++) {
    big_integer product = rhs;
    product *= big_digits[i];
    product.add(product_result, product_result, result.end() - product_result);
  }
  big_digits = result;
  remove_zeros();
  return *this;
}

void big_integer::divide(const big_integer& first, const big_integer& second, big_integer& quotient,
                         big_integer& remainder) {
  if (second.big_digits.empty()) {
    throw std::logic_error("division by zero");
  }

  quotient.sign = (first.sign == second.sign)? big_integer::POSITIVE : big_integer::NEGATIVE;

  int first_size = 32 * first.big_digits.size() - 32, second_size = 32 * second.big_digits.size() - 32;

  for (uint32_t i = first.big_digits.back(); i != 0; first_size++, i >>= 1)
    ;
  for (uint32_t i = second.big_digits.back(); i != 0; second_size++, i >>= 1)
    ;

  remainder = abs(first);

  if (first_size < second_size) {
    quotient = 0;
    return;
  }

  quotient.big_digits = std::vector<uint32_t>((first_size - second_size) / 32 + 1);

  big_integer shifted_second = abs(second << (first_size - second_size));
  for (int i = first_size - second_size; i >= 0; i--, shifted_second >>= 1) {
    if (remainder >= shifted_second) {
      remainder -= shifted_second;
      quotient.big_digits[i / 32] += 1 << (i % 32);
    }
  }
  quotient.remove_zeros();
  remainder.remove_zeros();
  if (quotient.sign == big_integer::NEGATIVE && !remainder.big_digits.empty()) {
    remainder = second - remainder;
  }
}

big_integer& big_integer::operator/=(const big_integer& rhs) {
  big_integer remainder;
  divide(*this, rhs, *this, remainder);
  return *this;
}

big_integer& big_integer::operator%=(const big_integer& rhs) {
  big_integer quotient;
  divide(*this, rhs, quotient, *this);
  return *this;
}

big_integer& big_integer::operator*=(uint32_t rhs) {
  uint64_t overflow = 0;
  for (size_t digit = 0; digit < big_digits.size(); digit++) {
    overflow += static_cast<uint64_t>(rhs) * static_cast<uint64_t>(big_digits[digit]);
    big_digits[digit] = static_cast<uint32_t>(overflow & UINT32_MAX);
    overflow = overflow >> 32;
  }
  if (overflow > 0) {
    big_digits.push_back(static_cast<uint32_t>(overflow & UINT32_MAX));
  }
  remove_zeros();
  return *this;
}

void big_integer::according_operator(const big_integer& rhs, uint32_t (*function)(uint32_t, uint32_t),
                                     uint32_t start_value) {
  for (size_t digit = 0; digit < rhs.big_digits.size() || digit < big_digits.size(); digit++) {
    uint32_t result = start_value;
    if (digit < rhs.big_digits.size()) {
      result = function(result, rhs.big_digits[digit]);
    }
    if (digit < big_digits.size()) {
      big_digits[digit] = function(big_digits[digit], result);
    } else {
      big_digits.push_back(result);
    }
  }
  remove_zeros();
}

big_integer& big_integer::operator&=(const big_integer& rhs) {
  according_operator(
      rhs, [](uint32_t a, uint32_t b) { return a & b; }, UINT32_MAX);
  return *this;
}

big_integer& big_integer::operator|=(const big_integer& rhs) {
  according_operator(
      rhs, [](uint32_t a, uint32_t b) { return a | b; }, 0);
  return *this;
}

big_integer& big_integer::operator^=(const big_integer& rhs) {
  according_operator(
      rhs, [](uint32_t a, uint32_t b) { return a ^ b; }, 0);
  return *this;
}

big_integer& big_integer::operator<<=(int rhs) {
  size_t new_digits = rhs / 32;
  size_t offset = rhs % 32;
  for (size_t i = 0; i <= new_digits + 1; i++) {
    big_digits.push_back(0);
  }
  for (size_t i = big_digits.size(); i > 0; i--) {
    if (i >= new_digits + 2) {
      big_digits[i - 1] =
          (big_digits[i - new_digits - 1] << offset) + (big_digits[i - new_digits - 2] >> (32 - offset));
    } else if (i == new_digits + 1) {
      big_digits[i - 1] = (big_digits[i - new_digits - 1] << offset);
    } else {
      big_digits[i - 1] = 0;
    }
  }
  remove_zeros();
  return *this;
}

big_integer& big_integer::operator>>=(int rhs) {
  size_t removed_digits = rhs / 32;
  size_t offset = rhs % 32;
  big_digits.push_back(0);
  for (size_t i = 0; i < big_digits.size() - 1; i++) {
    if (i + removed_digits < big_digits.size() - 1) {
      big_digits[i] =
          (big_digits[i + removed_digits] >> offset) + (big_digits[i + removed_digits + 1] << (32 - offset));
    } else {
      big_digits[i] = 0;
    }
  }
  remove_zeros();
  return *this;
}

big_integer big_integer::operator+() const {
  return big_integer(*this);
}

big_integer big_integer::operator-() const {
  big_integer result = *this;
  result.sign = !result.sign;
  return result;
}

big_integer big_integer::operator~() const {
  if (*this == big_integer(0)) {
    return -1;
  } else if (sign == POSITIVE) {
    return -(*this + 1);
  } else {
    return -(*this - 1);
  }
}

big_integer& big_integer::operator++() {
  *this += 1;
  return *this;
}

big_integer big_integer::operator++(int) {
  big_integer old = *this;
  ++*this;
  return old;
}

big_integer& big_integer::operator--() {
  *this -= 1;
  return *this;
}

big_integer big_integer::operator--(int) {
  big_integer old = *this;
  --*this;
  return old;
}

big_integer operator+(const big_integer& a, const big_integer& b) {
  return (big_integer(a) += b);
}

big_integer operator-(const big_integer& a, const big_integer& b) {
  return (big_integer(a) -= b);
}

big_integer operator*(const big_integer& a, const big_integer& b) {
  return (big_integer(a) *= b);
}

big_integer operator/(const big_integer& a, const big_integer& b) {
  return (big_integer(a) /= b);
}

big_integer operator%(const big_integer& a, const big_integer& b) {
  return (big_integer(a) %= b);
}

big_integer operator&(const big_integer& a, const big_integer& b) {
  return (big_integer(a) &= b);
}

big_integer operator|(const big_integer& a, const big_integer& b) {
  return (big_integer(a) |= b);
}

big_integer operator^(const big_integer& a, const big_integer& b) {
  return (big_integer(a) ^= b);
}

big_integer operator<<(const big_integer& a, int b) {
  return big_integer(a) <<= b;
}

big_integer operator>>(const big_integer& a, int b) {
  return big_integer(a) >>= b;
}

big_integer::RATIO big_integer::get_ratio(const big_integer& other) const {
  if (big_digits.size() == 0 && other.big_digits.size() == 0) {
    return RATIO::EQUAL;
  } else if (sign != other.sign) {
    return (sign == POSITIVE) ? RATIO::MORE : RATIO::LESS;
  } else if (big_digits.size() != other.big_digits.size()) {
    return (big_digits.size() > other.big_digits.size()) ? RATIO::MORE : RATIO::LESS;
  }
  for (int digit = static_cast<int>(big_digits.size()) - 1; digit >= 0; digit--) {
    if (big_digits[digit] != other.big_digits[digit]) {
      return (big_digits[digit] > other.big_digits[digit]) ? RATIO::MORE : RATIO::LESS;
    }
  }
  return RATIO::EQUAL;
}

bool operator==(const big_integer& a, const big_integer& b) {
  return a.get_ratio(b) == big_integer::RATIO::EQUAL;
}

bool operator!=(const big_integer& a, const big_integer& b) {
  return !(a == b);
}

bool operator<(const big_integer& a, const big_integer& b) {
  return a.get_ratio(b) == big_integer::RATIO::LESS;
}

bool operator>(const big_integer& a, const big_integer& b) {
  return a.get_ratio(b) == big_integer::RATIO::MORE;
}

bool operator<=(const big_integer& a, const big_integer& b) {
  return !(a > b);
}

bool operator>=(const big_integer& a, const big_integer& b) {
  return !(a < b);
}

big_integer abs(const big_integer& a) {
  big_integer result = a;
  result.sign = big_integer::POSITIVE;
  return result;
}

std::string to_string(const big_integer& a) {
  if (a.big_digits.size() == 0) {
    return "0";
  }
  std::vector<char> reversed_string;

  big_integer a_copy = a;

  while (a_copy.big_digits.size() > 0) {
    reversed_string.push_back(static_cast<char>(a_copy.divide(big_integer::RADIX)) + '0');
  }
  if (a.sign == big_integer::NEGATIVE) {
    reversed_string.push_back('-');
  }

  std::reverse(reversed_string.begin(), reversed_string.end());

  return std::string(reversed_string.begin(), reversed_string.end());
}

std::ostream& operator<<(std::ostream& out, const big_integer& a) {
  return out << to_string(a);
}
