#include <bits/stdc++.h>

using namespace std;

class snm {
public:
    snm(int n) : data_(n), count(n) {
        for (int i = 0; i < n; i++) {
            data_[i] = i;
        }
    }

    void merge(int a, int b) noexcept {
        int a_p = get_peak(a);
        int b_p = get_peak(b);
        if (a_p != b_p) {
            count --;
        }
        data_[a_p] = data_[b_p];
    }

    bool is_same(int a, int b) const noexcept {
        return get_peak(a) == get_peak(b);
    }

    int get_count() const noexcept {
        return count;
    }
private:
    mutable vector<int> data_;
    int count;

    int get_peak(int k) const noexcept {
        int l = k;
        while (data_[k] != k) {
            k = data_[k];
        }
        data_[l] = k;
        return k;
    }
};

class graph{
public:
    struct edge;

    struct node {
        int id;
        vector<edge*> edges;
    };

    struct edge {
        int id;
        int color;
        node* nodes[2] = {nullptr, nullptr};

        node* to(node* other) {
            if (other == nodes[0]) {
                return nodes[1];
            } else {
                return nodes[0];
            }
        }
    };

    graph(int n, int m) : nodes(n), edges() {
        edges.reserve(m);

        for (int i = 0; i < n; i++) {
            nodes[i].id = i;
        }
    }

    void add_edge(int node1, int node2, int color) {
        edges.emplace_back();
        edges.back().id = edges.size() - 1;
        edges.back().color = color;
        edges.back().nodes[0] = &nodes[node1];
        edges.back().nodes[1] = &nodes[node2];
        nodes[node1].edges.push_back(&edges.back());
        nodes[node2].edges.push_back(&edges.back());
    }

    set<int> get_correct_edges() {
        set<int> result;
        bool is_max = false;
        snm res_snm(nodes.size());
        set<int> res_color;
        while (!is_max) {
            set<int> X1, X2;
            auto it = result.begin();
            for (int i = 0; i < edges.size(); i++) {
                if (it != result.end() && *it == i) {
                    it++;
                    continue;
                }
                if (res_color.find(edges[i].color) == res_color.end()) {
                    X1.insert(i);
                }
                if (!res_snm.is_same(edges[i].nodes[0]->id, edges[i].nodes[1]->id)) {
                    X2.insert(i);
                }
            }
        }
    }

private:
    vector<node> nodes;
    vector<edge> edges;
};

int main() {
    int n, m;
    cin >> n >> m;
}