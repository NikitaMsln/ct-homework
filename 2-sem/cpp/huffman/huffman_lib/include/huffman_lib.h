#pragma once
#include <iostream>

namespace huffman {

void compress(std::istream&, std::ostream&);

void decompress(std::istream&, std::ostream&);
} // namespace huffman
