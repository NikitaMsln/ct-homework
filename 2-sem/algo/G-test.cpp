#include <iostream>
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

    ways_matrix(const size_t (&matrix)[3][3]) noexcept {
        set(matrix);
    }

    void set(const size_t (&matrix)[3][3]) noexcept {
        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                this->matrix[i][j] = matrix[i][j];
            }
        }
    }

    size_t &operator()(size_t i, size_t j) {
        assert(i < 3 && j < 3);
        return matrix[i][j];
    }

    const size_t &operator()(size_t i, size_t j) const {
        assert(i < 3 && j < 3);
        return matrix[i][j];
    }

    friend ways_matrix operator*(const ways_matrix &first, const ways_matrix &second) noexcept;

    ways_matrix &operator*=(const ways_matrix &other) noexcept {
        return (*this = *this * other);
    }

private:
    size_t matrix[3][3];
};

bool operator==(const ways_matrix &first, const ways_matrix &second) noexcept {
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            if (first(i, j) != second(i, j)) {
                return false;
            }
        }
    }
    return true;
}

ways_matrix operator*(const ways_matrix &first, const ways_matrix &second) noexcept {
    ways_matrix result;
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            for (size_t x = 0; x < 3; x++) {
                result(i, j) += first(i, x) * second(x, j);
            }
        }
    }
    return result;
}

std::ostream &operator<<(std::ostream &out, const ways_matrix &matrix) {
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            out << matrix(i, j) << " ";
        }
        out << std::endl;
    }
    return out;
}

int main() {
    ways_matrix full[10];
    full[0].set({
                        {0, 0, 1},
                        {1, 0, 1},
                        {0, 1, 1}
    });

    for (size_t i = 1; i < 10; i++) {
        full[i] = full[i - 1] * full[0];
        std::cout << i + 1 << std::endl << full[i];
    }
}