#include <iostream>
#include <thread>
#include <cassert>

class ways_matrix {
public:
    ways_matrix() {
        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                matrix[i][j] = 0;
            }
        }
    }
    ways_matrix(const size_t (& matrix)[3][3]) noexcept {
        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                this->matrix[i][j] = matrix[i][j];
            }
        }
    }

    size_t& operator()(size_t i, size_t j) {
        assert(i < 3 && j < 3);
        return matrix[i][j];
    }

    const size_t& operator()(size_t i, size_t j) const {
        assert(i < 3 && j < 3);
        return matrix[i][j];
    }

    friend ways_matrix operator+(const ways_matrix& first, const ways_matrix& second) noexcept;

    ways_matrix& operator+=(const ways_matrix& other) noexcept {
        return (*this = *this + other);
    }
private:
    size_t matrix[3][3];
};

bool operator==(const ways_matrix& first, const ways_matrix& second) noexcept {
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            if (first(i, j) != second(i, j)) {
                return false;
            }
        }
    }
    return true;
}

ways_matrix operator+(const ways_matrix& first, const ways_matrix& second) noexcept {
    ways_matrix result;
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            for (size_t x = 0; x < 3; x++) {
                for (size_t y = 0; y < 3; y++) {
                    if (x >= y) {
                        result(i, j) += first(i, x) * second(y, j);
                    }
                }
            }
        }
    }
    return result;
}

std::ostream& operator<<(std::ostream& out, const ways_matrix& matrix) {
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            out << "from " << i << " to " << j << ": " << matrix(i, j) << std::endl;
        }
    }
    return out;
}

int main() {
    // w3 without skips
    ways_matrix w3({{1, 1, 2}, {0, 1, 1}, {0, 0, 1}});
    // w4 without skips
    ways_matrix w4({{1, 2, 4}, {1, 1, 2}, {0, 1, 1}});
    // w6 without skips
    ways_matrix w6({{4, 7, 13}, {2, 4, 7}, {1, 2, 4}});

    // check correct
    std::cout << (w3 + w3 == w6);
    // check commutativity
    std::cout << (w3 + w6 == w6 + w3 && w4 + w3 == w3 + w4 && w6 + w4 == w4 + w6);
    // check associativity
    std::cout << (w3 + (w4 + w6) == (w3 + w4) + w6);

    // lets find w1 for i cell: w4 = w1 + w3
    auto f = [](size_t i, const ways_matrix& w3, const ways_matrix& w4) {
        for (size_t j = 1; j < 1'000'000'000; j++) {
            ways_matrix expected_w1({
                                            {(i & 1) * (j % 10),((i >> 1) & 1) * (j / 10 % 10), ((i >> 2) & 1) * (j / 100 % 10)},
                                            {((i >> 3) & 1) * (j / 1'000 % 10), ((i >> 4) & 1) * (j / 10'000 % 10), ((i >> 5) & 1) * (j / 100'000 % 10)},
                                            {((i >> 6) & 1) * (j / 1'000'000 % 10), ((i >> 7) & 1) * (j / 10'000'000 % 10), ((i >> 8) & 1) * (j / 100'000'000 % 10)}
                                    });

            if (w4 == expected_w1 + w3) {
                std::cout << "Found w1:" << std::endl << expected_w1;
                break;
            }
        }
    };
    std::thread *threads[1 << 10];
    for (size_t i = 1; i < (1 << 10); i++) {
        threads[i] = new std::thread(f, i, w3, w4);
    }

    for (size_t i = 1; i < (1 << 10); i++) {
        threads[i]->join();
        delete threads[i];
    }
}