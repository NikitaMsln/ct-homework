#include "test_iostream.h"

#include <gtest/gtest.h>
#include <huffman_lib.h>

#include <cstdlib>
#include <iostream>

TEST(coding_and_decoding, single_char) {
  for (size_t count = 0; count < 0x100000; count = (count << 1) + 1) {
    generated_istreambuf gen(generator<0>([](std::array<uint8_t, 0>&) { return 'a'; }, std::array<uint8_t, 0>()),
                             count);
    std::iostream in(&gen);

    std::vector<char> compressed;
    vector_streambuf compressed_buff(compressed);
    std::iostream compressed_stream(&compressed_buff);

    huffman::compress(in, compressed_stream);
    std::vector<char> res;
    vector_streambuf res_buff(res);
    std::ostream result(&res_buff);

    huffman::decompress(compressed_stream, result);
    EXPECT_TRUE(gen.equal(res));
  }
}

TEST(coding_and_decoding, all_single_chars) {
  std::array<uint8_t, 1> status;
  for (int ch = std::numeric_limits<char>::min(); ch <= std::numeric_limits<char>::max(); ch++) {
    status = {static_cast<uint8_t>(static_cast<char>(ch))};
    generated_istreambuf gen(
        generator<1>([](std::array<uint8_t, 1>& data) { return static_cast<char>(data[0]); }, status), 1000);
    std::iostream in(&gen);

    std::vector<char> compressed;
    vector_streambuf compressed_buff(compressed);
    std::iostream compressed_stream(&compressed_buff);

    huffman::compress(in, compressed_stream);
    std::vector<char> res;
    vector_streambuf res_buff(res);
    std::ostream result(&res_buff);

    huffman::decompress(compressed_stream, result);
    EXPECT_TRUE(gen.equal(res));
  }
}

TEST(coding_and_decoding, all_chars_in_text) {
  std::array<uint8_t, 1> status = {static_cast<uint8_t>(std::numeric_limits<uint8_t>::min() - 1)};
  for (size_t count = 1; count <= 1000; count = count * 10) {
    generated_istreambuf gen(generator<1>(
                                 [](std::array<uint8_t, 1>& data) {
                                   data[0] = (data[0] + 1) % 256;
                                   return static_cast<char>(data[0]);
                                 },
                                 status),
                             count * 256);
    std::iostream in(&gen);

    std::vector<char> compressed;
    vector_streambuf compressed_buff(compressed);
    std::iostream compressed_stream(&compressed_buff);

    huffman::compress(in, compressed_stream);
    std::vector<char> res;
    vector_streambuf res_buff(res);
    std::ostream result(&res_buff);

    huffman::decompress(compressed_stream, result);
    EXPECT_TRUE(gen.equal(res));
  }
}

TEST(coding_and_decoding, part_of_chars_in_text) {
  std::array<uint8_t, 2> status;
  for (size_t count = 2; count < 256; count++) {
    status[0] = static_cast<uint8_t>(count - 1);
    status[1] = status[0] + 1;
    generated_istreambuf gen(generator<2>(
                                 [](std::array<uint8_t, 2>& data) {
                                   data[0] = (data[0] + 1) % data[1];
                                   return static_cast<char>(data[0]);
                                 },
                                 status),
                             1000);
    std::iostream in(&gen);

    std::vector<char> compressed;
    vector_streambuf compressed_buff(compressed);
    std::iostream compressed_stream(&compressed_buff);

    huffman::compress(in, compressed_stream);
    std::vector<char> res;
    vector_streambuf res_buff(res);
    std::ostream result(&res_buff);

    huffman::decompress(compressed_stream, result);
    EXPECT_TRUE(gen.equal(res));
  }
}

TEST(coding_and_decoding, unbalanced_chars_in_text) {
  generated_istreambuf gen(generator<8>(
                               [](std::array<uint8_t, 8>& data) {
                                 uint64_t value = 0;
                                 for (size_t i = 0; i < 8; i++) {
                                   value += static_cast<uint64_t>(data[i]) << (i * 8);
                                 }
                                 size_t res = 64;
                                 for (size_t i = 0; i < 64; i++) {
                                   if ((value >> i) & 1 == 1) {
                                     res = i;
                                     break;
                                   }
                                 }
                                 value++;
                                 for (size_t i = 0; i < 8; i++) {
                                   data[i] = (value >> (i * 8)) & 0xff;
                                 }
                                 return static_cast<char>(res);
                               },
                               {0, 0, 0, 0, 0, 0, 0, 0}),
                           0x10'00'00);
  std::iostream in(&gen);

  std::vector<char> compressed;
  vector_streambuf compressed_buff(compressed);
  std::iostream compressed_stream(&compressed_buff);

  huffman::compress(in, compressed_stream);
  std::vector<char> res;
  vector_streambuf res_buff(res);
  std::ostream result(&res_buff);

  huffman::decompress(compressed_stream, result);
  EXPECT_TRUE(gen.equal(res));
}

TEST(coding_and_decoding, random_text) {
  for (size_t i = 0; i < 3; i++) {
    srand(i);
    std::vector<char> random_text(1'000'000, 0);
    for (size_t j = 0; j < random_text.size(); j++) {
      random_text[j] = static_cast<char>(rand() % 256);
    }
    vector_streambuf random_text_buff(random_text);
    std::iostream in(&random_text_buff);

    std::vector<char> compressed;
    vector_streambuf compressed_buff(compressed);
    std::iostream compressed_stream(&compressed_buff);

    huffman::compress(in, compressed_stream);
    std::vector<char> res;
    vector_streambuf res_buff(res);
    std::ostream result(&res_buff);

    huffman::decompress(compressed_stream, result);
    EXPECT_EQ(random_text, res);
  }
}
