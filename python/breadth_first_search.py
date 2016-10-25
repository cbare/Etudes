"""
Graph search
"""
from collections import deque
import random

class Node:
    def __init__(self, value, edges=None):
        self.value = value
        self.edges = [] if edges is None else edges

    def __repr__(self):
        return "Node('%s')" % str(self.value)

class Edge:
    def __init__(self, fr, to, weight):
        self.fr = fr
        self.to = to
        self.weight = weight

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
        for edge in node.edges:
            if edge.to not in visited:
                q.append(edge.to)
                visited.add(edge.to)
                backlinks[edge.to] = node
    else:
        raise ValueError("Target %s not found" % str(target))

def select(weights):
    r = random.random() * sum(weights)
    s = 0.0
    for k,w in enumerate(weights):
        s += w
        if r <= s:
            return k
    raise RuntimeError("select WTF from %s" % weights)

def harmonic_series(n):
    s = 0.0
    for i in range(1,(n+1)):
        s += 1.0/i
    return s

def attachment_likelihood(nodes):
    """
    preferentially attach to nodes with more edges, but at a
    decreasing rate for each additional connection.
    """
    return [harmonic_series(len(n.edges)) for n in nodes]

def generate_random_graph(values):
    nodes = []
    for v in values:
        node = Node(v)
        if len(nodes) > 0:
            p = 1.0
            nodes_remaining = list(nodes)
            while p > 0 and len(nodes_remaining)>0:
                i = select(attachment_likelihood(nodes_remaining))
                neighbor = nodes_remaining.pop(i)
                node.edges.append(Edge(node, neighbor, weight=random.random()))
                p = p * 2/3
                if random.random() >= p:
                    break
        nodes.append(node)
    return {node.value:node for node in nodes}

letters = "abcdefghijklmnopqrstuvwxyz"
g = generate_random_graph(letters)
for l in letters:
    node = g[l]
    print(node, ':', ','.join(str(e.to.value) for e in node.edges))

target, path = breadth_first_search(g['z'],'a')
print(target,path)

nv = 1000
g = generate_random_graph(range(nv))
print("\ngenerated random graph of size %d"%nv)
target, path = breadth_first_search(g[nv-1],0)
print(target,path)




