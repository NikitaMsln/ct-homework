#include <string>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <fstream>
#include <omp.h>

#ifndef SCHEDULE_PARAM
#define SCHEDULE_PARAM dynamic
#endif

#ifndef SCHEDULE_CHUNK_COUNT
#define SCHEDULE_CHUNK_COUNT 1
#endif

int get_int(std::string &s, int &result, int pos = 0);

int main(int argc, char **argv) {
    double start_time, end_time;

    if (argc < 4) {
        std::cout << "The number of arguments is less than 3: " << argc - 1 << std::endl;
        return 1;
    }

    int number_of_threads;

    try {
        number_of_threads = std::stoi(argv[1]);
    } catch (std::invalid_argument const &e) {
        std::cout << "Invalid number of threads (1st argument): " << e.what() << std::endl;
        return 1;
    } catch (std::out_of_range const &e) {
        std::cout << "Number of threads out of range (1st argument): " << e.what() << std::endl;
        return 1;
    }

    bool use_openmp = true;

    if (number_of_threads < 0) {
        use_openmp = false;
    } else if (number_of_threads > 0) {
        if (number_of_threads > 1000) {
            std::cout << "Number of threads is too high" << std::endl;
            return 1;
        }
        omp_set_num_threads(number_of_threads);
    } else {
        number_of_threads = omp_get_num_threads();
        omp_set_num_threads(number_of_threads);
    }

    std::ifstream in;

    in.open(argv[2], std::ios::binary | std::ios::in);

    if (!in.is_open()) {
        std::cout << "Failed to open file (2nd argument)" << argv[2] << std::endl;
        return 1;
    }



    std::ofstream out;

    out.open(argv[3], std::ios::binary | std::ios::out);

    if (!in.is_open()) {
        std::cout << "Failed to open file (3rd argument)" << argv[3] << std::endl;
        return 1;
    }

    int colors_count, width, height, pos;

    std::string line;
    std::getline(in, line);
    if (line != "P5") {
        std::cout << "Unsupported PNM file (2nd argument): " << argv[2] << std::endl;
        return 1;
    }
    line += "\n";
    out.write(line.c_str(), line.size());

    std::getline(in, line);
    pos = get_int(line, width);
    if (pos < 0 || pos >= line.size()) {
        std::cout << "Incorrect PNM file (2nd argument): " << argv[2] << std::endl;
        return 1;
    }

    pos = get_int(line, height, pos);
    if (pos < 0) {
        std::cout << "Incorrect PNM file (2nd argument): " << argv[2] << std::endl;
        return 1;
    }
    line += "\n";
    out.write(line.c_str(), line.size());

    std::getline(in, line);
    pos = get_int(line, colors_count);
    if (pos < 0 || colors_count != 255) {
        std::cout << "Incorrect PNM file (2nd argument): " << argv[2] << std::endl;
        return 1;
    }
    line += "\n";
    out.write(line.c_str(), line.size());

    int data_size = height * width;

    std::vector<int> data(data_size, 0);
    std::vector<int> hist(256, 0);

    const int BUFFER_SIZE = 4096;
    uint8_t buff[4096];

    for (int i = 0; !in.eof() && i < data_size;) {
        in.read((char *) buff, BUFFER_SIZE);
        for (int j = 0; j < in.gcount() && i < data_size; j++, i++) {
            data[i] = buff[j];
            hist[buff[j]]++;
        }
    }

    in.close();

    start_time = omp_get_wtime();

    std::vector<double> pref_prob(257, 0), math_expect(257, 0);

    for (int i = 1; i <= 256; i++) {
        double prob = (double) hist[i - 1] / (double) data_size;
        pref_prob[i] = pref_prob[i - 1] + prob;
        math_expect[i] = math_expect[i - 1] + prob * (double) (i - 1);
    }

    std::vector<unsigned int> max_borders(3, 0);
    double max_value = -1;
    if (use_openmp) {
        std::vector<std::vector<unsigned int>> local_max(number_of_threads, std::vector<unsigned int>(3, 0));
        std::vector<double> local_max_value(number_of_threads, -1);
        #pragma omp parallel default(none) shared(pref_prob, math_expect, local_max_value, local_max)
        {
            #pragma omp for schedule(SCHEDULE_PARAM, SCHEDULE_CHUNK_COUNT)
            for (int i = 0; i < 254; i++) {
                for (int j = i + 1; j < 255; j++) {
                    for (int k = j + 1; k < 256; k++) {
                        if (k <= j || j <= i) continue;
                        double prob[4], math_expects[4];
                        prob[0] = pref_prob[i];
                        prob[1] = pref_prob[j] - pref_prob[i];
                        prob[2] = pref_prob[k] - pref_prob[j];
                        prob[3] = pref_prob[256] - pref_prob[k];

                        math_expects[0] = math_expect[i];
                        math_expects[1] = math_expect[j] - math_expect[i];
                        math_expects[2] = math_expect[k] - math_expect[j];
                        math_expects[3] = math_expect[256] - math_expect[k];

                        double dispers = 0;
                        for (int l = 0; l < 4; l++) {
                            dispers += math_expects[l] * math_expects[l] / prob[l];
                        }
                        if (dispers > local_max_value[omp_get_thread_num()]) {
                            local_max_value[omp_get_thread_num()] = dispers;
                            local_max[omp_get_thread_num()][0] = i;
                            local_max[omp_get_thread_num()][1] = j;
                            local_max[omp_get_thread_num()][2] = k;
                        }
                    }
                }
            }
        }
        for (int i = 0; i < number_of_threads; i++) {
            if (local_max_value[i] > max_value) {
                max_value = local_max_value[i];
                max_borders = local_max[i];
            }
        }
    } else {
        for (int i = 0; i < 254; i++) {
            for (int j = i + 1; j < 255; j++) {
                for (int k = j + 1; k < 256; k++) {
                    if (k <= j || j <= i) continue;
                    double prob[4], math_expects[4];
                    prob[0] = pref_prob[i];
                    prob[1] = pref_prob[j] - pref_prob[i];
                    prob[2] = pref_prob[k] - pref_prob[j];
                    prob[3] = pref_prob[256] - pref_prob[k];

                    math_expects[0] = math_expect[i];
                    math_expects[1] = math_expect[j] - math_expect[i];
                    math_expects[2] = math_expect[k] - math_expect[j];
                    math_expects[3] = math_expect[256] - math_expect[k];

                    double dispers = 0;
                    for (int l = 0; l < 4; l++) {
                        dispers += math_expects[l] * math_expects[l] / prob[l];
                    }
                    if (dispers > max_value) {
                        max_value = dispers;
                        max_borders[0] = i;
                        max_borders[1] = j;
                        max_borders[2] = k;
                    }
                }
            }
        }
    }


    std::cout << max_borders[0] - 1 << " " << max_borders[1] - 1 << " " << max_borders[2] - 1 << std::endl;

    if (use_openmp) {
        #pragma omp parallel default(none) shared(data, max_borders, data_size)
        {
            #pragma omp for schedule(static)
            for (int i = 0; i < data_size; i++) {
                if (data[i] < max_borders[0]) data[i] = 0;
                else if (data[i] < max_borders[1]) data[i] = 84;
                else if (data[i] < max_borders[2]) data[i] = 170;
                else data[i] = 255;
            }
        }
    } else {
        for (int i = 0; i < data_size; i++) {
            if (data[i] < max_borders[0]) data[i] = 0;
            else if (data[i] < max_borders[1]) data[i] = 84;
            else if (data[i] < max_borders[2]) data[i] = 170;
            else data[i] = 255;
        }
    }

    end_time = omp_get_wtime();
    if (use_openmp)
        std::cout << "Time (" << number_of_threads << " thread(s)): " << (end_time - start_time) * 1000 << " ms" << std::endl;
    else
        std::cout << "Time (without OpenMP): " << (end_time - start_time) * 1000 << " ms" << std::endl;

    for (int i = 0; i < data_size;) {
        int write_size = BUFFER_SIZE;
        if (write_size > data_size - i) write_size = data_size - i;

        for (int j = 0; j < write_size; j++, i++) {
            buff[j] = data[i];
        }
        out.write((char *) buff, write_size);
    }

    out.close();

    return 0;
}

int get_int(std::string &s, int &result, int pos) {
    result = 0;

    for (; pos < s.size() && (s[pos] < '0' || s[pos] > '9'); pos++);

    if (pos >= s.size()) return -1;

    for (; pos < s.size() && s[pos] >= '0' && s[pos] <= '9'; pos++) result = result * 10 + s[pos] - '0';

    return pos;
}