"""
Binary Tree
"""
import sys,random

class Tree:
    def __init__(self, item, left=None, right=None):
        self.item = item
        self.left = left
        self.right = right

    def __repr__(self):
        return "Tree(%s)" % str(self.item)

def insert(tree, a):
    """
    Insert an item into the tree. This is functional style, so
    usage is:

       tree = insert(tree, 'foo')
    """
    if tree:
        if a < tree.item:
            tree.left = insert(tree.left, a)
        else:
            tree.right = insert(tree.right, a)
    else:
        tree = Tree(a)
    return tree

def delete(tree, a):
    """
    This is quirky 'cause we can usually get away with
    throwing away the return value except in the case
    where we delete the root node. Not ideal.
    """
    root = tree
    parent = None
    while tree is not None:
        if tree.item == a:
            if tree.left and tree.right:
                ## replace deleted node with min node in right subtree
                repl = tree.right
                if repl.left:
                    while repl.left:
                        repl_parent = repl
                        repl = repl.left
                    if repl.right:
                        repl_parent.left = repl.right
                    else:
                        repl_parent.left = None
                repl.left = tree.left
                if tree.right != repl:
                    repl.right = tree.right
            elif tree.left:
                ## replace deleted node with its left child
                repl = tree.left
            elif tree.right:
                ## replace deleted node with its right child
                repl = tree.right
            else:
                ## deleting a leaf node
                repl = None
            if parent:
                if a < parent.item:
                    parent.left = repl
                else:
                    parent.right = repl
            else:
                ## replace a root node, perhaps with None
                root = repl
            break
        elif a < tree.item:
            parent = tree
            tree = tree.left
        else:
            parent = tree
            tree = tree.right
    else:
        raise KeyError("Key %s not found" % str(a))

    return root


def print_in_order(tree):
    if tree:
        sys.stdout.write('(')
        print_in_order(tree.left)
        sys.stdout.write(str(tree.item))
        print_in_order(tree.right)
        sys.stdout.write(')')

def traverse_in_order(tree):
    """
    Generator that yields items in order. Uses yield from which is
    new as of Python 3.3.
    """
    if tree:
        yield from traverse_in_order(tree.left)
        yield tree.item
        yield from traverse_in_order(tree.right)

def min_node(tree):
    """
    Return the minimum node of the tree or raise a ValueError
    if the tree is empty.
    """
    if tree is None:
        raise ValueError("Can't take min of empty tree")
    if tree.left:
        return min_node(tree.left)
    return tree

def min(tree):
    """
    Return the minimum item in the tree or raise a ValueError
    if the tree is empty.
    """
    mt = min_node(tree)
    return mt.item if mt else None

def test(tree):
    if tree is None:
        return True

    result = True
    if tree.left:
        assert tree.left.item < tree.item
        result &= result and test(tree.left)
    if tree.right:
        assert tree.right.item >= tree.item
        result &= result and test(tree.right)
    return result



tree = None
for n in random.sample(range(20), 20):
    tree = insert(tree, n)

print("test(tree) =", test(tree))

print(", ".join(str(item) for item in traverse_in_order(tree)))

print('min(tree) =', min(tree))

print('\n')
print_in_order(tree)
print('\n')


t = Tree(1)
t = delete(t, 1)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5))
print_in_order(t)
print('\n')
t = delete(t, 5)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5), right=Tree(16))
print_in_order(t)
print('\n')
t = delete(t, 5)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5), right=Tree(16))
print_in_order(t)
print('\n')
t = delete(t, 16)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5), right=Tree(16))
print_in_order(t)
print('\n')
t = delete(t, 12)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5))
print_in_order(t)
print('\n')
t = delete(t, 12)
print_in_order(t)
print('\n')

t = Tree(3, right=Tree(12, left=Tree(5), right=Tree(16, Tree(15),Tree(17))))
print_in_order(t)
print('\n')
t = delete(t, 12)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5), right=Tree(16, Tree(15),Tree(17)))
print_in_order(t)
print('\n')
t = delete(t, 12)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5), right=Tree(16, right=Tree(17)))
print_in_order(t)
print('\n')
t = delete(t, 12)
print_in_order(t)
print('\n')

t = Tree(12, left=Tree(5), right=Tree(26, Tree(16, right=Tree(17, right=Tree(18))), Tree(27)))
print_in_order(t)
print('\n')
t = delete(t, 12)

print('-'*80)
for i in range(100):
    tree = None
    for n in random.sample(range(100), 20):
        tree = insert(tree, n)

    while tree:
        if random.random() > 2.0/3.0:
            tree = insert(tree, random.randint(0,99))
        else:
            tree = delete(tree, random.choice(list(traverse_in_order(tree))))
        print_in_order(tree)
        print("\n")

