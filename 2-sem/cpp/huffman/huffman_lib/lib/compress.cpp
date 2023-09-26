#include "hided_iostream_work.h"
#include "huffman_lib.h"

#include <array>
#include <iostream>

void huffman::compress(std::istream& in, std::ostream& out) {
  in.clear();
  in.seekg(0);
  buffered_istream buffered_in(in);
  std::array<size_t, huffman_tree::SIZE> hist{};
  hist.fill(0);
  while (!buffered_in.is_ended()) {
    uint8_t a = static_cast<uint8_t>(buffered_in.next_byte());
    hist[a]++;
  }

  huffman_tree tree(hist);
  auto header = tree.serialize();

  buffered_ostream buffered_out(out);

  for (size_t i = 0; i < 4; i++) {
    buffered_out.write_byte(static_cast<std::byte>((header.size() >> (8 * i)) & 0xff));
  }

  size_t count = 0;
  for (auto ch : hist) {
    count += ch;
  }

  for (size_t i = 0; i < 8; i++) {
    buffered_out.write_byte(static_cast<std::byte>((count >> (8 * i)) & 0xff));
  }
  buffered_out.flush();

  bit_writer bit_out(out);

  bit_out.write_bit_sequence(header);

  auto coding = tree.get_code();

  in.clear();
  in.seekg(0);
  while (!buffered_in.is_ended()) {
    bit_out.write_bit_sequence(coding[static_cast<uint8_t>(buffered_in.next_byte())]);
  }
  bit_out.flush();
}
