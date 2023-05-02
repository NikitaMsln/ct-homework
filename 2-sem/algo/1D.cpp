#include <bits/stdc++.h>

using namespace std;

class binary_tree {
private:
    struct node {
        int count = 0;
        node* (childs[2]) = {nullptr, nullptr};
    };
    node top;
    uint32_t set = 0;
    uint32_t set_value = 0;
    uint32_t reverse = 0;

    int count(uint32_t l, uint32_t r, node* nd, uint32_t res, int a) {
        if (nd == nullptr) return 0;
        if (l <= res && r >= res - 1 + (1 << (a + 1))) {
            return nd->count;
        } else if (r < res || l >= res + (1 << (a + 1))) {
            return 0;
        }
        if (set & (1 << a)) {
            if (nd->childs[0] != nullptr && nd->childs[1] != nullptr) {
                merge(nd->childs[0], nd->childs[1]);
                nd->childs[0] = nullptr;
            }
            if (set_value & (1 << a)) {
                res = res | (1 << a);
            } else {
                res = res & (~(1 << a));
            }
            return count(l, r, nd->childs[0], res, a-1) + count(l, r, nd->childs[1], res, a-1);
        } else {
            uint32_t res1, res0;
            if (reverse & (1 << a)) {
                res0 = res | (1 << a);
                res1 = res & (~(1 << a));
            } else {
                res0 = res & (~(1 << a));
                res1 = res | (1 << a);
            }
            return count(l, r, nd->childs[0], res0, a-1) + count(l, r, nd->childs[1], res1, a-1);
        }
    }

    void merge(node* from, node* to) {
        if (from->childs[0] != nullptr) {
            if (to->childs[0] == nullptr) {
                to->childs[0] = new node;
            }
            merge(from->childs[0], to->childs[0]);
        }
        if (from->childs[1] != nullptr) {
            if (to->childs[1] == nullptr) {
                to->childs[1] = new node;
            }
            merge(from->childs[1], to->childs[1]);
        }
        to->count += from->count;
    }
public:
    void add(uint32_t a) {
        node* last = &top;
        for (int i = 19; i >= 0; i--) {
            uint32_t bit = (a >> i) & 1;
            if (last->childs[bit] == nullptr) {
                last->childs[bit] = new node;
            }
            last = last->childs[bit];
            last->count++;
        }
    }

    void xorall(uint32_t a) {
        reverse = a ^ reverse;
        set_value = a ^ set_value;
    }

    void andall(uint32_t a) {
        set = set | (~a);
        set_value = set_value & a;
    }

    int count(uint32_t l, uint32_t r) {
        return count(l, r, &top, 0, 19);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    int n, q;
    cin >> n >> q;
    binary_tree tree;
    for (int i = 0; i < n; i++) {
        uint32_t a;
        cin >> a;
        tree.add(a);
    }
    while (q--) {
        string command;
        cin >> command;
        if (command == "xor") {
            int a;
            cin >> a;
            tree.xorall(a);
        } else if (command == "and") {
            int a;
            cin >> a;
            tree.andall(a);
        } else {
            int l, r;
            cin >> l >> r;
            cout << tree.count(l, r) << "\n";
        }
    }
}