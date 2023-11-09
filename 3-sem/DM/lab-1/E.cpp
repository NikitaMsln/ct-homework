#include <bits/stdc++.h>

using namespace std;


struct node {
    uint32_t index;
    set<uint32_t> adj;
};

int main() {
    uint32_t n;
    cin >> n;
    vector<node> nodes(n);
    vector<node*> ordered_nodes(n);

    for (uint32_t i = 0; i < n; i++) {
        nodes[i].index = i + 1;
        ordered_nodes[i] = &nodes[i];
    }

    for (uint32_t i = 0; i < n - 1; i++) {
        uint32_t u, v;
        cin >> u >> v;
        nodes[u - 1].adj.insert(v);
        nodes[v - 1].adj.insert(u);
    }

    std::sort(ordered_nodes.begin(), ordered_nodes.end(), [](const node* a, const node* b) {
        return a->adj.size() < b->adj.size() || (a->adj.size() == b->adj.size() && a->index < b->index);
    });

    priority_queue<uint32_t, vector<uint32_t>, std::greater<>> res;
    for (auto ptr : ordered_nodes) {
        if (ptr->adj.size() > 1) {
            break;
        }
        res.push(ptr->index);
    }

    for (uint32_t i = 0; i < n - 2; i++) {
        uint32_t now_n = res.top();
        res.pop();
        node* adj = &nodes[*nodes[now_n - 1].adj.begin() - 1];
        std::cout << adj->index << " ";
        adj->adj.erase(now_n);
        if (adj->adj.size() == 1) {
            res.push(adj->index);
        }
    }
    cout << "\n";
}