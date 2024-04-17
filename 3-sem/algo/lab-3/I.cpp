#include <bits/stdc++.h>

using namespace std;

class bor {
private:
    struct node {
        int childs[27];
        int suf_links[27];
        int deep = 0;
        int parent;
        int suf_link = -1;
        int up = -1;
        char char_to_parent;
        bool is_leaf = false;
        int left = 1'000'000'000;
        int right = -1;
        vector<int*> lefts;
        vector<int*> rights;

        node(int parent, char ch) : parent(parent), char_to_parent(ch) {
            for (int i = 0; i < 27; i++) {
                childs[i] = -1;
                suf_links[i] = -1;
            }
        }
    };
public:
    bor() {
        nodes.emplace_back(-1, ' ');
    }

    void insert(const string& s, int* left, int* right) {
        int cur = 0;
        for (int i = 0; i < s.size(); i++) {
            if (nodes[cur].childs[s[i] - 'a'] == -1) {
                nodes[cur].childs[s[i] - 'a'] = nodes.size();
                nodes.emplace_back(cur, s[i]);
                nodes.back().deep = nodes[cur].deep + 1;
            }
            cur = nodes[cur].childs[s[i] - 'a'];
        }
        nodes[cur].lefts.push_back(left);
        nodes[cur].rights.push_back(right);
        nodes[cur].is_leaf = true;
        leafs.push_back(cur);
    }

    void contains_sub(const string& s) {
        int cur = 0;
        for (int i = 0; i < s.size(); i++) {
            nodes[cur].left = min(i, nodes[cur].left);
            nodes[cur].right = max(i, nodes[cur].right);
            cur = get_link(cur, s[i]);
        }
        nodes[cur].left = min((int)s.size(), nodes[cur].left);
        nodes[cur].right = max((int)s.size(), nodes[cur].right);
        for (int i = 0; i < nodes.size(); i++) {
            int leaf = i;
            int l = nodes[leaf].left;
            int r = nodes[leaf].right;
            while (leaf != 0 && r != -1) {
                for (auto* p : nodes[leaf].lefts) (*p) = min(l - nodes[leaf].deep, *p);
                for (auto* p : nodes[leaf].rights) (*p) = max(r - nodes[leaf].deep, *p);
                leaf = get_up(leaf);
            }
        }
    }

private:
    vector<node> nodes;
    vector<int> leafs;

    int get_link(int nod, char c) {
        node& curr = nodes[nod];
        int ind = c - 'a';
        if (curr.suf_links[ind] == -1) {
            if (curr.childs[ind] != -1) {
                curr.suf_links[ind] = curr.childs[ind];
            } else if (nod == 0) {
                curr.suf_links[ind] = 0;
            } else {
                curr.suf_links[ind] = get_link(get_suf_link(nod), c);
            }
        }
        return curr.suf_links[ind];
    }

    int get_up(int nod) {
        node& curr = nodes[nod];
        if (curr.up == -1) {
            int suf_l = get_suf_link(nod);
            if (nodes[suf_l].is_leaf) {
                curr.up = suf_l;
            } else if (suf_l == 0) {
                curr.up = 0;
            } else {
                curr.up = get_up(get_suf_link(nod));
            }
        }
        return curr.up;
    }

    int get_suf_link(int nod) {
        node& curr = nodes[nod];
        if (curr.suf_link == -1) {
            if (nod == 0 || curr.parent == 0) {
                curr.suf_link = 0;
            } else {
                curr.suf_link = get_link(get_suf_link(curr.parent), curr.char_to_parent);
            }
        }
        return curr.suf_link;
    }
};

int main() {
    int n;
    cin >> n;
    bor sb;
    vector<pair<int, int>> bounds(n, {1'000'000'000, -1});
    for (int i = 0; i < n; i++) {
        string p;
        cin >> p;
        sb.insert(p, &bounds[i].first, &bounds[i].second);
    }
    string t;
    cin >> t;
    sb.contains_sub(t);
    for (int i = 0; i < n; i++) {
        if (bounds[i].second == -1) {
            cout << "-1 -1\n";
        } else {
            cout << bounds[i].first << " " << bounds[i].second << "\n";
        }
    }
}
