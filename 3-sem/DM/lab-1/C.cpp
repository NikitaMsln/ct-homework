#include <bits/stdc++.h>

using namespace std;

struct node {
    node* next = nullptr;
    node* prev = nullptr;
    uint32_t index;
};

node* insert(node* left, node* right, uint32_t value) {
    node* new_n = new node;
    new_n->index = value;
    right->prev = left->next = new_n;
    new_n->prev = left;
    new_n->next = right;
    return new_n;
}

node* push_back(node* last, uint32_t value) {
    node* new_n = new node;
    new_n->index = value;
    last->next = new_n;
    new_n->prev = last;
    return new_n;
}

node* push_front(node* first, uint32_t value) {
    node* new_n = new node;
    new_n->index = value;
    first->prev = new_n;
    new_n->next = first;
    return new_n;
}

void clear(node* first) {
    while (first != nullptr) {
        auto f_c = first;
        first = first->next;
        delete f_c;
    }
}

void print(const vector<uint32_t>& v) {
    for (auto e : v) {
        cout << e << " ";
    }
    cout << "\n";
}

bool request(uint32_t i, uint32_t j) {
    cout << "1 " << i << " " << j << "\n";
    cout.flush();
    string s;
    cin >> s;
    return s[0] == 'Y';
}

vector<uint32_t> solution(uint32_t n, const function<bool(uint32_t, uint32_t)>& req) {
    vector<uint32_t> res(n + 1, 0);
    for (uint32_t i = 0; i <= n; i++) {
        res[i] = i;
    }
    stable_sort(std::next(res.begin()), res.end(), req);
    return res;
}

class tester {
public:
    tester(uint32_t count = 1000) : n(count), edges(vector<vector<bool>>(count, vector<bool>(count, false))) {
        std::srand(std::time(nullptr));
        for (uint32_t i = 0; i < n; i++) {
            for (uint32_t j = i + 1; j < n; j++) {
                edges[i][j] = !(edges[j][i] = (rand() % 3 == 0));
            }
        }
    }

    void print_edges() {
        cout << "\t";
        for (uint32_t i = 0; i < n; i++) {
            cout << (i + 1) << "\t";
        }
        cout << "\n";
        for (uint32_t i = 0; i < n; i++) {
            cout << (i + 1) << ":\t";
            for (uint32_t j = 0; j < n; j++) {
                cout << edges[i][j] << "\t";
            }
            cout << "\n";
        }
    }

    bool test() {
        auto r = req(edges);
        vector<uint32_t> res = solution(n, r);
        vector<uint32_t> errors;
        for (uint32_t i = 2; i < res.size(); i++) {
            if (!edges[res[i - 1] - 1][res[i] - 1]) {
                errors.push_back(i);
            }
        }
        if (!errors.empty() || r.get_counter() > 10000) {
            cout << "ERROR:\n";
            cout << "edges:\n";
            print_edges();
            cout << "\nresult:\n";
            print(res);
            cout << "\nerrors indexes:\n";
            for (auto i : errors) {
                cout << i - 1 << " ";
            }
            cout << "\nrequests count:\n";
            cout << r.get_counter();
            cout << "\n";
            return false;
        }
        return true;
    }
private:
    struct req {
    public:
        req(const vector<vector<bool>>& edg) : edg_(edg) {}

        bool operator()(uint32_t i, uint32_t j) {
            cnt++;
            return edg_[i - 1][j - 1];
        }

        size_t get_counter() {
            return cnt;
        }

    private:
        const vector<vector<bool>>& edg_;
        size_t cnt = 0;
    };

    uint32_t n = 100;
    vector<vector<bool>> edges = vector<vector<bool>>(n, vector<bool>(n, false));
};

void testing() {
    bool cont = true;
    uint32_t cnt = 1;
    while (cont) {
        if (cnt++ % 100 == 0) {
            cout << "TEST " << cnt - 1 << "\n";
        }
        cont = tester().test();
    }
}

int main() {
    uint32_t n;
    cin >> n;
    print(solution(n, request));
    //testing();
}