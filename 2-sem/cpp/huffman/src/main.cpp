#include <huffman_lib.h>

#include <fstream>
#include <string>

static void print_help();

int main(int argc, char** argv) {
  if (argc <= 1) {
    std::cerr << "Unable to run util without arguments (call \"--help\")" << std::endl;
    return 1;
  }
  bool help = false;
  bool is_compress = false;
  bool is_decompress = false;
  std::string input_file, output_file;
  for (size_t i = 1; i < argc; i++) {
    std::string value(argv[i]);
    if (value == "--help") {
      help = true;
    } else if (value == "--compress") {
      is_compress = true;
    } else if (value == "--decompress") {
      is_decompress = true;
    } else if (value == "--input") {
      i++;
      if (i >= argc) {
        std::cerr << "Path to input file is not found (enter path after \"--input\")" << std::endl;
        return 1;
      }
      input_file = argv[i];
    } else if (value == "--output") {
      i++;
      if (i >= argc) {
        std::cerr << "Path to output file is not found (enter path after \"--output\")" << std::endl;
        return 1;
      }
      output_file = argv[i];
    } else {
      std::cerr << "Unsupported option: \"" << value << '\"' << std::endl;
      return 1;
    }
  }

  if (help) {
    print_help();
    return 0;
  }

  if (is_compress && is_decompress) {
    std::cerr << "Entered 2 modes: choose compress or decompress file" << std::endl;
    return 1;
  }

  if (!is_compress && !is_decompress) {
    std::cerr << "Not entered mod: choose compress or decompress file" << std::endl;
    return 1;
  }

  if (input_file.empty()) {
    std::cerr << "Path to input file is not found (enter path after \"--input\")" << std::endl;
    return 1;
  }
  if (output_file.empty()) {
    if (is_compress) {
      output_file = input_file + ".huf";
    } else {
      std::cerr << "Path to output file is not found (enter path after \"--input\")" << std::endl;
      return 1;
    }
  }

  std::ifstream in;
  std::ofstream out;

  try {
    in.open(input_file, std::ios_base::in | std::ios_base::binary);
    if (!in.is_open()) {
      throw std::ios_base::failure("Opening of \"" + input_file + "\" is failed");
    }
  } catch (std::exception& e) {
    std::cerr << "Opening input file \"" << input_file << "\" is failed: " << e.what() << std::endl;
    return 1;
  }

  try {
    out.open(output_file, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    if (!out.is_open()) {
      throw std::ios_base::failure("Opening of \"" + output_file + "\" is failed");
    }
  } catch (std::exception& e) {
    std::cerr << "Opening output file \"" << output_file << "\" is failed: " << e.what() << std::endl;
    return 1;
  }

  if (is_compress) {
    try {
      huffman::compress(in, out);
    } catch (std::exception& e) {
      std::cerr << "Compressing is failed: " << e.what() << std::endl;
      return 1;
    }
  } else {
    try {
      huffman::decompress(in, out);
    } catch (std::exception& e) {
      std::cerr << "Decompressing is failed: " << e.what() << std::endl;
      return 1;
    }
  }
  return 0;
}

static void print_help() {
  std::cout << "NAME:" << std::endl;
  std::cout << "\thuffman-tool - compress abd decompress files with huffman code" << std::endl;
  std::cout << "SYNOPSIS:" << std::endl;
  std::cout << "\thuffman-tool [mode] --input [path to input file] --output [path to output file]" << std::endl;
  std::cout << "MODES:" << std::endl;
  std::cout << "\t--compress - compress file with huffman code" << std::endl;
  std::cout << "\t--decompress - decompress file with huffman code" << std::endl;
}
