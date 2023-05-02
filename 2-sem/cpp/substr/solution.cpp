#include <cstdio>
#include <cstdlib>
#include <cstring>

bool find_prefix(FILE* in, const char* substring, const size_t* prefix, size_t size);

void prefix_function(const char* string, size_t size, size_t* result);

int main(int argc, const char* argv[]) {
  if (argc != 3) {
    fprintf(stderr, "Expected 2 arguments: filename and substring");
    return EXIT_FAILURE;
  }

  FILE* in = fopen(argv[1], "rb");
  if (in == NULL) {
    perror("Opening file error");
    return EXIT_FAILURE;
  }

  size_t substring_size = strlen(argv[2]);

  size_t* prefix = static_cast<size_t*>(malloc(substring_size * sizeof(size_t)));
  if (prefix == NULL) {
    perror("Memory allocation error");
    if (fclose(in) != 0) {
      perror("Closing file error");
    }
    return EXIT_FAILURE;
  }

  prefix_function(argv[2], substring_size, prefix);

  bool is_correct = find_prefix(in, argv[2], prefix, substring_size);

  free(prefix);

  if (fclose(in) != 0) {
    perror("Closing file error");
    return EXIT_FAILURE;
  }

  if (!is_correct) {
    perror("Reading file error");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

size_t update_index(size_t index, const char* substring, const size_t* prefix, char source) {
  while (index > 0 && source != substring[index]) {
    index = prefix[index - 1];
  }
  if (source == substring[index]) {
    index++;
  }
  return index;
}

bool find_prefix(FILE* in, const char* substring, const size_t* prefix, size_t size) {
  size_t now_c;
  size_t j = 0;
  while ((now_c = fgetc(in)) != EOF) {
    j = update_index(j, substring, prefix, now_c);

    if (j == size) {
      fprintf(stdout, "Yes");
      return true;
    }
  }
  if (ferror(in)) {
    return false;
  }
  fprintf(stdout, "No");
  return true;
}

void prefix_function(const char* string, size_t size, size_t* result) {

  result[0] = 0;

  for (size_t i = 1; i < size; i++) {
    result[i] = update_index(result[i - 1], string, result, string[i]);
  }
}
