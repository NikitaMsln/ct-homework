#include <bits/stdc++.h>

using namespace std;

class bor {
private:
    struct node {
        int childs[27];
        int suf_links[27];
        int parent;
        int suf_link = -1;
        int up = -1;
        char char_to_parent;
        bool is_leaf = false;
        set<int> id;

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

    void insert(const string& s, int id) {
        int cur = 0;
        for (int i = 0; i < s.size(); i++) {
            if (nodes[cur].childs[s[i] - 'a'] == -1) {
                nodes[cur].childs[s[i] - 'a'] = nodes.size();
                nodes.emplace_back(cur, s[i]);
            }
            cur = nodes[cur].childs[s[i] - 'a'];
        }
        nodes[cur].id.insert(id);
        nodes[cur].is_leaf = true;
    }

    set<int> contains_sub(const string& s) {
        int cur = 0;
        set<int> res;
        for (auto c : s) {
            res.merge(nodes[cur].id);
            nodes[cur].is_leaf = false;
            nodes[cur].up = -1;
            int next = get_link(cur, c);
            cur = next;
            while (next != 0) {
                next = get_up(next);
                res.merge(nodes[next].id);
                nodes[next].is_leaf = false;
                nodes[next].up = -1;
            }
        }
        if (!nodes[cur].id.empty()) {
            res.merge(nodes[cur].id);
        }
        return res;
    }

private:
    vector<node> nodes;

    int get_link(int nod, char c) {
        if (nodes[nod].suf_links[c - 'a'] == -1) {
            if (nodes[nod].childs[c - 'a'] != -1) {
                nodes[nod].suf_links[c - 'a'] = nodes[nod].childs[c - 'a'];
            } else if (nod == 0) {
                nodes[nod].suf_links[c - 'a'] = 0;
            } else {
                nodes[nod].suf_links[c - 'a'] = get_link(get_suf_link(nod), c);
            }
        }
        return nodes[nod].suf_links[c - 'a'];
    }

    int get_up(int nod) {
        if (nodes[nod].up == -1) {
            int suf_l = get_suf_link(nod);
            if (nodes[suf_l].is_leaf) {
                nodes[nod].up = suf_l;
            } else if (suf_l == 0) {
                nodes[nod].up = 0;
            } else {
                nodes[nod].up = get_up(get_suf_link(nod));
            }
        }
        return nodes[nod].up;
    }

    int get_suf_link(int nod) {
        if (nodes[nod].suf_link == -1) {
            if (nod == 0 || nodes[nod].parent == 0) {
                nodes[nod].suf_link = 0;
            } else {
                nodes[nod].suf_link = get_link(get_suf_link(nodes[nod].parent), nodes[nod].char_to_parent);
            }
        }
        return nodes[nod].suf_link;
    }
};

int main() {
    int n;
    cin >> n;
    bor sb;
    for (int i = 0; i < n; i++) {
        string p;
        cin >> p;
        sb.insert(p, i);
    }
    vector<bool> finded(n, false);
    string t;
    cin >> t;
    auto r = sb.contains_sub(t);
    for (auto x : r) finded[x] = true;
    for (int i = 0; i < n; i++) {
        if (finded[i]) {
            cout << "YES\n";
        } else {
            cout << "NO\n";
        }
    }
}