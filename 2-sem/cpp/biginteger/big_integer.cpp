#include "big_integer.h"

#include <algorithm>
#include <compare>
#include <ostream>
#include <stdexcept>
#include <vector>

big_integer::big_integer() = default;

big_integer::big_integer(const big_integer& other) = default;

big_integer::big_integer(int a) : big_integer(static_cast<long long>(a)) {}

big_integer::big_integer(long a) : big_integer(static_cast<long long>(a)) {}

big_integer::big_integer(long long a) : sign((a >= 0) ? POSITIVE : NEGATIVE) {
  unsigned long long b = (a < 0) ? -static_cast<unsigned long long>(a) : a;
  while (b > 0) {
    big_digits.push_back(b);
    b >>= 32;
  }
  remove_zeros();
}

big_integer::big_integer(unsigned int a) : big_integer(static_cast<unsigned long long>(a)) {}

big_integer::big_integer(unsigned long a) : big_integer(static_cast<unsigned long long>(a)) {}

big_integer::big_integer(unsigned long long a) : sign(POSITIVE) {
  while (a > 0) {
    big_digits.push_back(a);
    a >>= 32;
  }
  remove_zeros();
}

void big_integer::add(uint32_t rhs) {
  if (sign == POSITIVE || big_digits.empty()) {
    module_add(rhs);
  } else {
    module_subtract(rhs);
  }
}

void big_integer::subtract(uint32_t rhs) {
  if (sign == POSITIVE || big_digits.empty()) {
    module_subtract(rhs);
  } else {
    module_add(rhs);
  }
}

void big_integer::module_add(uint32_t rhs) {
  uint64_t overflow = rhs;
  for (size_t digit = 0; overflow > 0; digit++) {
    if (digit < big_digits.size()) {
      overflow += static_cast<uint64_t>(big_digits[digit]);
      big_digits[digit] = overflow;
      overflow = overflow >> 32;
    } else {
      big_digits.push_back(overflow);
      return;
    }
  }
}

void big_integer::module_subtract(uint32_t rhs) {
  if (big_digits.empty()) {
    big_digits.push_back(rhs);
    sign = NEGATIVE;
    return;
  } else if (big_digits[0] < rhs && big_digits.size() == 1) {
    big_digits[0] = rhs - big_digits[0];
    sign = !sign;
    return;
  }
  uint64_t overflow = 1;
  for (size_t digit = 0; digit < big_digits.size(); digit++) {
    overflow = (static_cast<uint64_t>(1) << 32) + overflow - 1;
    if (digit == 0) {
      overflow -= rhs;
    }
    overflow += big_digits[digit];
    big_digits[digit] = static_cast<uint32_t>(overflow & UINT32_MAX);
    overflow = overflow >> 32;
  }
}

big_integer::big_integer(const std::string& str) : sign(POSITIVE) {
  if (str.length() == 0 || str == "-") {
    throw std::invalid_argument("string in big_integer(std::string) is empty");
  }
  size_t i = 0;
  bool result_sign = POSITIVE;
  if (str[i] == '-') {
    result_sign = NEGATIVE;
    i++;
  }
  uint32_t digit = 0;
  uint32_t big_radix = 1;
  for (; i < str.size(); i++) {
    if (str[i] < '0' || str[i] > '9') {
      throw std::invalid_argument("string is unable to parse to big_integer: " + str);
    }
    digit = digit * RADIX + (str[i] - '0');
    big_radix *= RADIX;
    if (big_radix == BIG_RADIX) {
      multiply(BIG_RADIX);
      add(digit);
      digit = 0;
      big_radix = 1;
    }
  }
  multiply(big_radix);
  add(digit);
  sign = result_sign;
}

big_integer::~big_integer() = default;

void big_integer::remove_zeros() {
  while (!big_digits.empty() && big_digits.back() == 0) {
    big_digits.pop_back();
  }
}

big_integer& big_integer::operator=(const big_integer& other) {
  if (this == &other) {
    return *this;
  }
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
    *result = static_cast<uint32_t>(overflow);
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
    *result = static_cast<uint32_t>(overflow);
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
  big_digits.resize(std::max(rhs.big_digits.size(), big_digits.size()));
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
  sign = !sign;
  *this += rhs;
  sign = !sign;
  return *this;
}

big_integer& big_integer::operator*=(const big_integer& rhs) {
  if (big_digits.empty()) {
    return *this;
  }
  sign = (sign == rhs.sign) ? POSITIVE : NEGATIVE;
  size_t first_size = big_digits.size();
  big_digits.resize(big_digits.size() + rhs.big_digits.size() + 1);
  for (size_t i = first_size; i > 0; i--) {
    uint64_t mul_digit = big_digits[i - 1];
    big_digits[i - 1] = 0;
    first_size--;

    uint64_t mul_overflow = 0;
    uint64_t add_overflow = 0;
    size_t digit;

    for (digit = 0; mul_overflow > 0 || add_overflow > 0 || digit < rhs.big_digits.size(); digit++) {
      if (digit < rhs.big_digits.size()) {
        mul_overflow += mul_digit * static_cast<uint64_t>(rhs.big_digits[digit]);
      }
      add_overflow += (mul_overflow & UINT32_MAX) + static_cast<uint64_t>(big_digits[first_size + digit]);
      big_digits[first_size + digit] = static_cast<uint32_t>(add_overflow);
      mul_overflow = mul_overflow >> 32;
      add_overflow = add_overflow >> 32;
    }
  }
  remove_zeros();
  return *this;
}

void big_integer::divide(const big_integer& first, const big_integer& second, big_integer& quotient,
                         big_integer& remainder) {
  if (second.big_digits.empty()) {
    throw std::logic_error("division by zero");
  }
  if (first.big_digits.empty()) {
    quotient = 0;
    remainder = 0;
    return;
  }

  bool remainder_sign = first.sign;

  quotient.sign = (first.sign == second.sign) ? POSITIVE : NEGATIVE;

  size_t first_size = 32 * first.big_digits.size() - 32;
  size_t second_size = 32 * second.big_digits.size() - 32;

  for (uint32_t i = first.big_digits.back(); i != 0; i >>= 1) {
    first_size++;
  }
  for (uint32_t i = second.big_digits.back(); i != 0; i >>= 1) {
    second_size++;
  }

  remainder = abs(first);

  if (first_size < second_size) {
    quotient = 0;
    remainder.sign = remainder_sign;
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
  remainder.sign = remainder_sign;
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

void big_integer::multiply(uint32_t rhs) {
  uint64_t overflow = 0;
  for (size_t digit = 0; digit < big_digits.size(); digit++) {
    overflow += static_cast<uint64_t>(rhs) * static_cast<uint64_t>(big_digits[digit]);
    big_digits[digit] = static_cast<uint32_t>(overflow);
    overflow = overflow >> 32;
  }
  if (overflow > 0) {
    big_digits.push_back(static_cast<uint32_t>(overflow));
  }
  remove_zeros();
}

uint32_t big_integer::evaluate_according_digits(uint32_t start_value, uint32_t value, bool sign, bool& decrement,
                                                uint32_t (*function)(uint32_t, uint32_t)) {
  if (sign == NEGATIVE) {
    if (!decrement) {
      if (value > 0) {
        value--;
        decrement = true;
        return function(start_value, ~value);
      } else {
        return function(start_value, 0);
      }
    } else {
      return function(start_value, ~value);
    }
  } else {
    return function(start_value, value);
  }
}

void big_integer::according_operator(const big_integer& rhs, uint32_t (*function)(uint32_t, uint32_t),
                                     uint32_t start_value) {
  bool first_decrement = false;
  bool second_decrement = false;
  size_t size = std::max(big_digits.size(), rhs.big_digits.size()) + 1;
  bool first_sign = (sign == NEGATIVE && big_digits.size() > 0) ? NEGATIVE : POSITIVE;
  bool second_sign = (rhs.sign == NEGATIVE && rhs.big_digits.size() > 0) ? NEGATIVE : POSITIVE;
  big_digits.resize(size);
  for (size_t i = 0; i < size; i++) {
    uint32_t result = start_value;
    result = evaluate_according_digits(result, big_digits[i], first_sign, first_decrement, function);
    big_digits[i] = evaluate_according_digits(result, (i < rhs.big_digits.size()) ? rhs.big_digits[i] : 0, second_sign,
                                              second_decrement, function);
  }
  if (big_digits.back() >> 31) {
    sign = NEGATIVE;
    add(1);
    for (size_t i = 0; i < big_digits.size(); i++) {
      big_digits[i] = ~big_digits[i];
    }
  } else {
    sign = POSITIVE;
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
  big_digits.resize(big_digits.size() + new_digits + 1);
  for (size_t i = big_digits.size(); i > 0; i--) {
    if (i >= new_digits + 2 && offset != 0) {
      big_digits[i - 1] =
          (big_digits[i - new_digits - 1] << offset) + (big_digits[i - new_digits - 2] >> (32 - offset));
    } else if (i >= new_digits + 1) {
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
      if (offset != 0) {
        big_digits[i] =
            (big_digits[i + removed_digits] >> offset) + (big_digits[i + removed_digits + 1] << (32 - offset));
      } else {
        big_digits[i] = big_digits[i + removed_digits];
      }
    } else {
      big_digits[i] = 0;
    }
  }
  if (sign == NEGATIVE) {
    *this -= 1;
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
  if (big_digits.empty()) {
    return -1;
  } else if (sign == POSITIVE) {
    return -(*this + 1);
  } else {
    return -(*this - 1);
  }
}

big_integer& big_integer::operator++() {
  add(1);
  return *this;
}

big_integer big_integer::operator++(int) {
  big_integer old = *this;
  ++*this;
  return old;
}

big_integer& big_integer::operator--() {
  subtract(1);
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

std::strong_ordering big_integer::operator<=>(const big_integer& rhs) const {
  if (big_digits.empty() && rhs.big_digits.empty()) {
    return std::strong_ordering::equal;
  } else if (sign != rhs.sign) {
    return (sign == POSITIVE) ? std::strong_ordering::greater : std::strong_ordering::less;
  } else if (big_digits.size() != rhs.big_digits.size()) {
    bool more = big_digits.size() > rhs.big_digits.size();
    return ((more && sign == POSITIVE) || (!more && sign == NEGATIVE)) ? std::strong_ordering::greater
                                                                       : std::strong_ordering::less;
  }
  for (size_t digit = big_digits.size(); digit > 0; digit--) {
    if (big_digits[digit - 1] != rhs.big_digits[digit - 1]) {
      bool more = big_digits[digit - 1] > rhs.big_digits[digit - 1];
      return ((more && sign == POSITIVE) || (!more && sign == NEGATIVE)) ? std::strong_ordering::greater
                                                                         : std::strong_ordering::less;
    }
  }
  return std::strong_ordering::equal;
}

bool big_integer::operator==(const big_integer& rhs) const {
  return operator<=>(rhs) == std::strong_ordering::equal;
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
  std::string reversed_string;

  big_integer a_copy = a;

  while (a_copy.big_digits.size() > 0) {
    uint32_t digit = a_copy.divide(big_integer::BIG_RADIX);
    for (uint32_t i = 1; i < big_integer::BIG_RADIX; i *= big_integer::RADIX) {
      reversed_string.push_back(static_cast<char>(digit % big_integer::RADIX) + '0');
      digit /= big_integer::RADIX;
    }
  }
  while (reversed_string.back() == '0') {
    reversed_string.pop_back();
  }
  if (a.sign == big_integer::NEGATIVE) {
    reversed_string.push_back('-');
  }

  std::reverse(reversed_string.begin(), reversed_string.end());

  return reversed_string;
}

std::ostream& operator<<(std::ostream& out, const big_integer& a) {
  return out << to_string(a);
}
