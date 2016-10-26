"""
Graph search
"""
from collections import deque
from math import log
from heapq import heappush, heappop
from functools import total_ordering
import random
import time
import io


@total_ordering
class Node:
    def __init__(self, value, edges=None):
        self.value = value
        self.edges = [] if edges is None else edges

    def __lt__(self, other):
        return self.value < other.value

    def __eq__(self, other):
        return self.value == other.value

    def __hash__(self):
        return hash(self.value)

    def __repr__(self):
        return "Node('%s')" % str(self.value)

class Edge:
    def __init__(self, fr, to, weight):
        self.fr = fr
        self.to = to
        self.weight = weight

def path_to_string(path):
    out = []
    prev_node = path[0]
    out.append(str(prev_node))
    w = 0.0
    for node in path[1:]:
        for edge in prev_node.edges:
            if edge.to == node:
                out.append("%0.2f"%edge.weight)
                w += edge.weight
        out.append(str(node))
        prev_node = node
    out.append(" total cost=%0.2f"%w)
    return '--'.join(out)


def get_path(backlinks, node):
    path = []
    path_node = node
    while path_node is not None:
        path.append(path_node)
        path_node = backlinks.get(path_node, None)
    return list(reversed(path))


def depth_first_search(node, target):
    visited = set([node])
    backlinks = {node:None}

    def dfsr(node, target):
        if node.value == target:
            return node
        for edge in node.edges:
            if edge.to not in visited:
                backlinks[edge.to] = node
                target = dfsr(edge.to, target)
                if target is not None:
                    return target
        return None

    target = dfsr(node, target)
    path = get_path(backlinks, target)
    return target, path


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


def dijkstras(node, target):
    visited = {node:0}
    q = [(0,node)]
    backlinks = {node:None}

    while len(q) > 0:
        priority, node = heappop(q)
        if node.value == target:
            return node, priority, get_path(backlinks, node)

        for edge in node.edges:
            new_priority = priority + edge.weight
            if new_priority < visited.get(edge.to, float('inf')):
                heappush(q, (new_priority, edge.to))
                visited[edge.to] = new_priority
                backlinks[edge.to] = node
    else:
        raise ValueError("Target %s not found" % str(target))


##------------------------------------------------------------
## Generate random graphs
##------------------------------------------------------------
def select(weights):
    """
    select a node with probability proportional to its "weight"
    """
    r = random.random() * sum(weights)
    s = 0.0
    for k,w in enumerate(weights):
        s += w
        if r <= s:
            return k
    raise RuntimeError("select WTF from %s" % weights)

def attachment_likelihood(node):
    """
    preferentially attach to nodes with more edges, but at a
    decreasing rate for each additional connection.
    """
    return log(len(node.edges)+1)+1.0

def generate_random_graph(values):
    attachment_likelihoods = []
    nodes = []
    for v in values:
        node = Node(v)
        if len(nodes) > 0:
            p = 1.0
            while p > 0:
                i = select(attachment_likelihoods)
                neighbor = nodes[i]
                dup = False
                for edge in node.edges:
                    if neighbor == edge.to:
                        break
                else:
                    w = random.uniform(0,10)
                    node.edges.append(Edge(node, neighbor, weight=w))
                    neighbor.edges.append(Edge(neighbor, node, weight=w))
                    attachment_likelihoods[i] = attachment_likelihood(neighbor)
                p = p * 2/3
                if random.random() >= p:
                    break
        nodes.append(node)
        attachment_likelihoods.append(attachment_likelihood(node))
    return {node.value:node for node in nodes}


##------------------------------------------------------------
## Testing
##------------------------------------------------------------
letters = "abcdefghijklmnopqrstuvwxyz"
g = generate_random_graph(letters)
for l in letters:
    node = g[l]
    print(node, ':', ','.join(str(e.to.value) for e in node.edges))

target, path = breadth_first_search(g['z'], 'a')
print("bfs found target:", target,path)

target,path = depth_first_search(g['z'], 'a')
print("dfs found target:", target, path)

nv = 5000

start_time = time.time()
g = generate_random_graph(range(nv))
print("\ngenerated random graph of size %d"%nv)
print("--- %0.6f seconds ---" % (time.time() - start_time))

start_time = time.time()
target, path = breadth_first_search(g[nv-1],0)
print("\nbfs found:", target, path)
print("--- %0.6f seconds ---" % (time.time() - start_time))

start_time = time.time()
target, cost, path = dijkstras(g[nv-1], 0)
print("\ndijkstra's found:", target, "%0.2f"%cost, path)
print("--- %0.6f seconds ---" % (time.time() - start_time))

print("\n", path_to_string(path))



