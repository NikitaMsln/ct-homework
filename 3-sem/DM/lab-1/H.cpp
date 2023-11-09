#include <bits/stdc++.h>

using namespace std;

class polynomial {
public:
    polynomial() noexcept = default;
    polynomial(int64_t c) : data_(1, c) {}
    polynomial(int64_t x, int64_t c) : data_(2, c) {
        data_[1] = x;
    }

    size_t degree() const noexcept {
        return data_.size();
    }

    friend polynomial operator+(const polynomial& lhs, const polynomial& rhs) {
        polynomial res;
        res.data_ = vector<int64_t>(max(lhs.degree(), rhs.degree()), 0);
        for (size_t i = 0; i < res.data_.size(); i++) {
            if (lhs.data_.size() > i) {
                res.data_[i] += lhs.data_[i];
            }
            if (rhs.data_.size() > i) {
                res.data_[i] += rhs.data_[i];
            }
        }
        res.check_degree();
        return res;
    }

    friend polynomial operator*(const polynomial& lhs, const polynomial& rhs) {
        polynomial res;
        polynomial tmp = rhs;
        for (size_t i = 0; i < lhs.degree(); i++) {
            polynomial tmpc = tmp;
            for (uint32_t j = 0; j < tmpc.degree(); j++) {
                tmpc.data_[j] *= lhs.data_[i];
            }
            res = res + tmpc;
            tmp.data_.insert(tmp.data_.begin(), 0);
        }
        return res;
    }

    friend polynomial operator-(const polynomial& lhs, const polynomial& rhs) {
        return lhs + (-1 * rhs);
    }

    void print() const {
        if (data_.size() == 0) {
            cout << "0\n0\n";
            return;
        }
        cout << degree() - 1 << "\n";
        for (int i = data_.size(); i > 0; i--) {
            cout << data_[i - 1] << " ";
        }
        cout << "\n";
    }

private:
    void check_degree() noexcept {
        while (data_.back() == 0) {
            data_.pop_back();
        }
    }

    vector<int64_t> data_;
};

polynomial K[11];

void init_K() {
    K[0] = 1;
    for (int64_t i = 1; i < 11; i++) {
        K[i] = K[i - 1] * polynomial(1, -i + 1);
    }
}

class graph {
public:
    graph(vector<vector<bool>>&& matrix) : matrix_(std::move(matrix)) {}

    polynomial get_chromatic_polynomial() const {
        for (int i = 0; i < matrix_.size() - 1; i++) {
            for (int j = i + 1; j < matrix_.size(); j++) {
                if (!matrix_[i][j]) {
                    graph has_edge(*this);
                    has_edge.matrix_[i][j] = has_edge.matrix_[j][i] = true;
                    graph join_node = join_nodes(i, j);
                    return has_edge.get_chromatic_polynomial() + join_node.get_chromatic_polynomial();
                }
            }
        }
        return K[matrix_.size()];
    }


private:
    graph join_nodes(int u, int v) const {
        int new_n = matrix_.size() - 1;
        vector<vector<bool>> new_matrix(new_n, vector<bool>(new_n, false));
        if (u > v) {
            swap(u, v);
        }
        for (int i = 0; i < matrix_.size() - 1; i++) {
            for (int j = i + 1; j < matrix_.size(); j++) {
                if (matrix_[i][j]) {
                    int ni = i, nj = j;
                    if (ni == v) {
                        ni = u;
                    } else if (ni > v) {
                        ni--;
                    }
                    if (nj == v) {
                        nj = u;
                    } else if (nj > v) {
                        nj--;
                    }
                    new_matrix[ni][nj] = new_matrix[nj][ni] = true;
                }
            }
        }
        return graph(std::move(new_matrix));
    }

    vector<vector<bool>> matrix_;
};

int main() {
    init_K();

    int n, m;
    cin >> n >> m;

    vector<vector<bool>> matrix(n, vector<bool>(n, false));
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        matrix[u - 1][v - 1] = matrix[v - 1][u - 1] = true;
    }
    polynomial res = graph(std::move(matrix)).get_chromatic_polynomial();
    res.print();
}