"""
A function accepts an array of strings. Each string is an
"id1 id2 ts". The IDs are numbers which identify people. ts
is a timestamp of when those two people became friends. The
function also accepts the total number of people in the
group. Return the timestamp of when all people in the group
are connected into a single "graph". The strings are in
time order (ordered by ts). The set is guaranteed to have a
complete, single graph by the end.
"""

def _parse_connection(connection):
    """
    Parse a string of the form "{id1} {id2} {ts}" for
    example: "1 2 12345678"
    """
    return tuple(int(field) for field in connection.split(' '))

class Node:
    """
    Represents a tree node for a variant of the Union-Find
    algorithm. Nodes know their parents and nodes that are
    their own parents are root nodes.
    """
    def __init__(self, id, parent=None):
        self.id = id
        self.parent = parent if parent else self

    def is_root(self):
        return self.parent==self

    def find_root(self):
        node = self
        while not node.is_root():
            node = node.parent
        return node

def when_connected(connections, n):
    ## Here we're assuming the ids are in range [0-n) so we
    ## can use a list. If that's not the case, we'll need to
    ## use a dict instead.
    components = [Node(i) for i in range(n)]
    for connection in connections:
        id1, id2, ts = _parse_connection(connection)
        node1 = components[id1]
        parent1 = node1.find_root()
        node2 = components[id2]
        parent2 = node2.find_root()
        if parent1 != parent2:
            parent2.parent = parent1
            n -= 1
            if n==1:
                return ts
    raise RuntimeError('Not a connected graph')

connections = [
    "0 1 1",
    "0 2 2",
    "1 2 3",
]
print(when_connected(connections, 3))

connections = [
    "0 1 1",
    "0 2 2",
    "1 2 3",
    "1 3 4",
    "1 4 5",
    "5 6 6",
    "6 5 6",
    "6 7 7",
    "0 7 8",
]
print(when_connected(connections, 8))

## try a disconnected example
connections = [
    "0 1 1",
    "0 2 2",
    "1 2 3",
    "1 3 4",
    "1 4 5",
    "5 6 6",
]
try:
    print(when_connected(connections, 7))
except RuntimeError as e:
    print('Error expected')
    print(e)


