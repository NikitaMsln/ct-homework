import numpy as np
import random

def get_weight(graph, way):
    res = graph[way[-1]][way[0]]
    for i in range(1, len(way)):
        res += graph[way[i - 1]][way[i]]
    return res

def get_random_elements(l, M):
    res = []
    for i in range(M):
        res.append(l[random.randrange(0, len(l))])
    return res

print("Enter filename with adjacency matrix of graph")
graph = []
with open(input(), encoding="utf-8") as f:
    for line in f:
        graph.append([float(x) for x in line.split()])

print("Enter N")
N = int(input())
print("Enter M")
M = int(input())
print("Enter nu")
nu = float(input())

permutations = []
for i in range(N):
    permutations.append([x for x in np.random.permutation(len(graph))])

print("Enter count of generations")
count = int(input())

best_weight = get_weight(graph, permutations[0])
best_perm = permutations[0] + []

for i in range(count):
    population = get_random_elements(permutations, M)
    for j in range(M):
        v = random.uniform(0, 1)
        # mutation
        if v < nu:
            first = random.randrange(0, len(graph))
            second = random.randrange(0, len(graph))
            while abs(first - second) <= 1 or (first == 0 and second == len(graph) - 1) or (second == 0 and first == len(graph) - 1):
                second = random.randrange(0, len(graph))
            
            mutant = population[j] + []

            # changing edges

            l = min(first, second)
            r = max(first, second)
            for k in range(r - l):
                lk = k + l
                rk = r - k - 1
                if lk >= rk:
                    break
                v = mutant[lk]
                mutant[lk] = mutant[rk]
                mutant[rk] = v
            population.append(mutant)

        # crossover
        if random.uniform(0, 1) < nu:
            other = random.randrange(0, M)
            while other == j:
                other = random.randrange(0, M)
            
            crossover = [-1] * len(graph)
            
            for k in range(len(population[j])):
                crossover[population[other][k]] = population[j][k]

    population.sort(key=lambda x: get_weight(graph, x))
    
    local_best_weight = get_weight(graph, population[0])
    if best_weight > local_best_weight:
        best_weight = local_best_weight
        best_perm = population[0] + []

    permutations = []
    for x in population:
        if len(permutations) == 0 or permutations[-1] != x:
            permutations.append(x)
        if len(permutations) == N:
            break

print("best way:", " ".join([str(x + 1) for x in best_perm]))
print("weight:", best_weight)
