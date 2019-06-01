import random

EMPTY = tuple()
NUM_PROJECTS = 1000

class Folder:
    def __init__(self, id, children=None):
        self.id = id
        self.children = children or []


class Leaf:
    def __init__(self, id):
        self.id = id
    @property
    def children(self):
        return EMPTY
    

class Sequence:
    def __init__(self, i=100001):
        self.starting_value = i
        self.i = i
    def __next__(self):
        tmp = self.i
        self.i += 1
        return tmp
    def ids_given(self):
        return self.i-self.starting_value


def subtree(n, m, depth=1):
    return (
        [Leaf(next(ids)) for i in range(random.randint(0, n))] + 
        [Folder(next(ids), subtree(n, m, depth-1)) for i in range(random.randint(0, m if depth > 1 else 0))]
    ) if depth > 0 else []


ids = Sequence()
projects = {}
for i in range(NUM_PROJECTS):
    project_id = next(ids)
    project = Folder(project_id, subtree(100, 5, 7))
    projects[project_id] = project

print(f'Total nodes created: {ids.ids_given()}')

