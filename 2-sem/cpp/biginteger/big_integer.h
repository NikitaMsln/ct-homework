#pragma once

#include <compare>
#include <iosfwd>
#include <string>
#include <vector>

struct big_integer {
public:
  static const bool POSITIVE = false;
  static const bool NEGATIVE = true;
  static const uint32_t RADIX = 10;
  static const uint32_t BIG_RADIX = 1000000000;

private:
  bool sign = POSITIVE;
  std::vector<uint32_t> big_digits;
  void remove_zeros();
  void add(std::vector<uint32_t>::iterator result, std::vector<uint32_t>::const_iterator other,
           size_t other_size) const;
  void subtract(std::vector<uint32_t>::iterator result, std::vector<uint32_t>::const_iterator other,
                size_t other_size) const;
  static uint32_t evaluate_according_digits(uint32_t start_value, uint32_t value, bool sign, bool& decrement,
                                            uint32_t (*function)(uint32_t, uint32_t));
  void according_operator(const big_integer& rhs, uint32_t (*function)(uint32_t, uint32_t), uint32_t start_value);
  uint32_t divide(uint32_t rhs);
  static void divide(const big_integer& first, const big_integer& second, big_integer& quotilent,
                     big_integer& remainder);

  void multiply(uint32_t rhs);
  void add(uint32_t rhs);
  void module_add(uint32_t rhs);
  void subtract(uint32_t rhs);
  void module_subtract(uint32_t rhs);

public:
  big_integer();
  big_integer(const big_integer& other);
  big_integer(int a);
  big_integer(long a);
  big_integer(long long a);
  big_integer(unsigned int a);
  big_integer(unsigned long a);
  big_integer(unsigned long long a);
  big_integer(const std::string& str);
  ~big_integer();

  big_integer& operator=(const big_integer& other);

  big_integer& operator+=(const big_integer& rhs);
  big_integer& operator-=(const big_integer& rhs);
  big_integer& operator*=(const big_integer& rhs);
  big_integer& operator/=(const big_integer& rhs);
  big_integer& operator%=(const big_integer& rhs);

  big_integer& operator&=(const big_integer& rhs);
  big_integer& operator|=(const big_integer& rhs);
  big_integer& operator^=(const big_integer& rhs);

  big_integer& operator<<=(int rhs);
  big_integer& operator>>=(int rhs);

  big_integer operator+() const;
  big_integer operator-() const;
  big_integer operator~() const;

  big_integer& operator++();
  big_integer operator++(int);

  big_integer& operator--();
  big_integer operator--(int);

  std::strong_ordering operator<=>(const big_integer& rhs) const;
  bool operator==(const big_integer& rhs) const;

  friend big_integer abs(const big_integer& a);

  friend std::string to_string(const big_integer& a);
};

big_integer operator+(const big_integer& a, const big_integer& b);
big_integer operator-(const big_integer& a, const big_integer& b);
big_integer operator*(const big_integer& a, const big_integer& b);
big_integer operator/(const big_integer& a, const big_integer& b);
big_integer operator%(const big_integer& a, const big_integer& b);

big_integer operator&(const big_integer& a, const big_integer& b);
big_integer operator|(const big_integer& a, const big_integer& b);
big_integer operator^(const big_integer& a, const big_integer& b);

big_integer operator<<(const big_integer& a, int b);
big_integer operator>>(const big_integer& a, int b);

std::string to_string(const big_integer& a);
std::ostream& operator<<(std::ostream& out, const big_integer& a);
