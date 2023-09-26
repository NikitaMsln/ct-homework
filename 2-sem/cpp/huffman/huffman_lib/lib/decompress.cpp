#include "hided_iostream_work.h"
#include "huffman_lib.h"

#include <iostream>

void huffman::decompress(std::istream& in, std::ostream& out) {
  in.clear();
  in.seekg(0);
  buffered_istream buffered_in(in);

  size_t header_size = 0;
  for (size_t i = 0; i < 4; i++) {
    header_size += static_cast<size_t>(buffered_in.next_byte()) << (8 * i);
  }

  if (header_size == 0) {
    return;
  }

  size_t count = 0;
  for (size_t i = 0; i < 8; i++) {
    count += static_cast<size_t>(buffered_in.next_byte()) << (8 * i);
  }

  bit_reader bit_in(buffered_in);

  bit_sequence serialized_tree = bit_in.get_bit_sequence(header_size);

  huffman_tree tree(serialized_tree);
  auto encoding = tree.begin();
  buffered_ostream buffered_out(out);
  for (size_t i = 0; i < count;) {
    if (encoding.is_leaf()) {
      buffered_out.write_byte(static_cast<std::byte>(encoding.get_value()));
      encoding = tree.begin();
      i++;
    } else {
      encoding.next(bit_in.get_bit());
    }
  }
  buffered_out.flush();
}
