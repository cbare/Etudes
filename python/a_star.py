"""
A-star heuristic search algorithm

Amit Patel (@redblobgames) wrote an awesome
[Introduction to A*](http://www.redblobgames.com/pathfinding/a-star/introduction.html).
"""
from queue import PriorityQueue
from math import sqrt
import itertools
import random

class Node:
    def __init__(self, value, x, y, edges=None):
        self.value = value
        self.x = x
        self.y = y
        self.edges = [] if edges is None else edges

    def __repr__(self):
        return "Node(%s)" % str(self.value)

class Edge:
    def __init__(self, fr, to, weight):
        self.fr = fr
        self.to = to
        self.weight = weight

    def __repr__(self):
        return "Edge(%s, %s, %0.2f)"%(self.fr, self.to, self.weight)


def euclidean_distance(a,b):
    return sqrt( (a.x-b.x)**2 + (a.y-b.y)**2 )

def reconstruct_path(node, back):
    """
    follow back pointers to reconstruct path
    """
    results = []
    while node is not None:
        results.append(node)
        node = back[node]
    return results

def search(start, goal, heuristic):
    """
    A* heuristic search
    """
    q = PriorityQueue()
    q.put((0, start))
    back = {}
    cost_so_far = {}
    back[start] = None
    cost_so_far[start] = 0

    while not q.empty():
        priority, node = q.get()

        if node == goal:
            return list(reversed(reconstruct_path(node, back)))
   
        for edge in node.edges:
            new_cost = cost_so_far[node] + edge.weight
            if edge.to not in cost_so_far or new_cost < cost_so_far[edge.to]:
                cost_so_far[edge.to] = new_cost
                back[edge.to] = node
                priority = new_cost + heuristic(goal, edge.to)
                q.put((priority, edge.to))


def generate_random_graph(values, edge_p=0.10):
    nodes = []
    for value in values:
        nodes.append(Node(value, random.uniform(0,100), random.uniform(0,100)))
    for a,b in itertools.combinations(nodes,2):
        if random.random() < edge_p:
            ## Our heuristic, euclidean distance, is not allowed to over
            ## estimate the true distance, so we make the true distance
            ## along an edge randomly a bit bigger.
            d = euclidean_distance(a, b) + random.lognormvariate(0, 10)
            a.edges.append(Edge(a, b, d))
            b.edges.append(Edge(b, a, d))
    return nodes


g = generate_random_graph(range(0,100))
path = search(g[0], g[99], euclidean_distance)
print(path)





