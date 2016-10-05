"""
Graph search
"""
from collections import deque, defaultdict
import io
import random

class Node:
    def __init__(self, value, neighbors=None):
        self.value = value
        self.neighbors = [] if neighbors is None else neighbors

    def __repr__(self):
        return "Node('%s')" % str(self.value)

def get_path(backlinks, node):
    path = []
    path_node = node
    while path_node is not None:
        path.append(path_node)
        path_node = backlinks.get(path_node, None)
    return list(reversed(path))

def breadth_first_search(start, target):
    visited = set()
    q = deque()
    q.append(start)
    visited.add(start)
    backlinks = {}

    while len(q) > 0:
        node = q.popleft()
        if node.value == target:
            return node, get_path(backlinks, node)
        for neighbor in node.neighbors:
            if neighbor not in visited:
                q.append(neighbor)
                visited.add(neighbor)
                backlinks[neighbor] = node
    else:
        raise ValueError("Target %s not found" % str(target))

def select(weights):
    r = random.random() * sum(weights)
    s = 0.0
    for k,w in enumerate(weights):
        s += w
        if r <= s:
            return k

def harmonic_series(n):
    s = 0.0
    for i in range(1,(n+1)):
        s += 1.0/i
    return s

def generate_random_graph(values):
    nodes = []
    for v in values:
        node = Node(v)
        if len(nodes) > 0:
            p = 1.0
            while p > 0:
                i = select([harmonic_series(len(n.neighbors)) for n in nodes])
                if nodes[i] not in node.neighbors:
                    node.neighbors.append(nodes[i])
                    nodes[i].neighbors.append(node)
                p = p / 2
                if random.random() >= p:
                    break
        nodes.append(node)
    return {node.value:node for node in nodes}

letters = "abcdefghijklmnopqrstuvwxyz"
g = generate_random_graph(letters)
for l in letters:
    node = g[l]
    print(node, ':', ','.join(str(n.value) for n in node.neighbors))

target, path = breadth_first_search(g['a'],'z')
print(target,path)

