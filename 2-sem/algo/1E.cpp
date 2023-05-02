#include <bits/stdc++.h>

using namespace std;

template <typename T>
class splay_tree {
private:
    struct node {
        int value;
        T info;
        node *parent = nullptr, *left = nullptr, *right = nullptr;
    };
    node* top = nullptr;
    void rotate_left(node* nd) {
        node *p = nd->parent, *r = nd->right;
        if (p != nullptr) {
            if (p->left == nd) {
                p->left = r;
            } else {
                p->right = r;
            }
        }
        node *tmp = r->left;
        r->left = nd;
        nd->right = tmp;
        nd->parent = r;
        r->parent = p;
        if (nd->right != nullptr) {
            nd->right->parent = nd;
        }
    }

    void rotate_right(node* nd) {
        node *p = nd->parent, *l = nd->left;
        if (p != nullptr) {
            if (p->right == nd) {
                p->right = l;
            } else {
                p->left = l;
            }
        }
        node *tmp = l->right;
        l->right = nd;
        nd->left = tmp;
        nd->parent = l;
        l->parent = p;
        if (nd->left != nullptr) {
            nd->left->parent = nd;
        }
    }

    void splay(node* nd) {
        while (nd->parent != nullptr) {
            if (nd == nd->parent->left) {
                if (nd->parent->parent == nullptr) {
                    rotate_right(nd->parent);
                } else if (nd->parent == nd->parent->parent->left) {
                    rotate_right(nd->parent->parent);
                    rotate_right(nd->parent);
                } else {
                    rotate_right(nd->parent);
                    rotate_left(nd->parent);
                }
            } else {
                if (nd->parent->parent == nullptr) {
                    rotate_left(nd->parent);
                } else if (nd->parent == nd->parent->parent->right) {
                    rotate_left(nd->parent->parent);
                    rotate_left(nd->parent);
                } else {
                    rotate_left(nd->parent);
                    rotate_right(nd->parent);
                }
            }
        }
    }

    node* merge(node* first, node* second) {
        if (first == nullptr) return second;
        if (second == nullptr) return first;

        node* first_max = first;
        while (first_max->right != nullptr) first_max = first_max->right;
        splay(first_max);
        first_max->right = second;
        return first_max;
    }
public:
    T* find(int x) {
        node *cur = top, *old_cur= nullptr;
        while (cur != nullptr && cur->value != x) {
            old_cur = cur;
            if (cur->value > x) {
                cur = cur->left;
            } else {
                cur = cur->right;
            }
        }
        if (cur != nullptr) {
            if (old_cur != nullptr) splay(old_cur);
            return &cur->info;
        } else {
            splay(cur);
            return nullptr;
        }
    }

    void add(int x, T& info) {
        if (top == nullptr) {
            top = new node;
            top->value = x;
            top->info = info;
            return;
        }

        node* cur = top;
        while (cur->value != x) {
            if (cur->value > x) {
                if (cur->left == nullptr) {
                    cur->left = new node;
                    cur->left->parent = cur;
                    cur->left->value = x;
                    cur->left->info = info;
                }
                cur = cur->left;
            } else {
                if (cur->right == nullptr) {
                    cur->right = new node;
                    cur->right->parent = cur;
                    cur->right->value = x;
                    cur->right->info = info;
                }
                cur = cur->right;
            }
        }
        splay(cur);
    }

    void remove(int x) {
        node* cur = top;
        while (cur != nullptr) {
            if (cur->value > x) {
                if (cur->left == nullptr) {
                    return;
                }
                cur = cur->left;
            } else if (cur->value < x) {
                if (cur->right == nullptr) {
                    return;
                }
                cur = cur->right;
            } else {
                node *r = cur->right, *l = cur->left, *p = cur->parent;
                bool is_left = p->left == cur;
                r->parent = nullptr;
                l->parent = nullptr;
                delete cur;
                node* n = merge(l, r);
                if (is_left) {
                    p->left = n;
                } else {
                    p->right = n;
                }
                n->parent = p;
                return;
            }
        }
    }
};

int main() {
    splay_tree<spl<int>> ;
    int n, m;
    cin >> n >> m;

}