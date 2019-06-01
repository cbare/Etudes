"""
Fun with linked lists.

Given a linked list and n, find the nth element from the
end of the list.
"""

class Node:
    """An element of a linked list"""
    def __init__(self, value, nxt=None):
        self.value = value
        self.nxt = nxt

    def __repr__(self):
        return "Node("+str(self.value)+","+("None" if self.nxt is None else str(self.nxt)) + ")"

def make_linked_list(lst):
    """
    Turn a Python list (or any iterable) into a linked list
    """
    iterator = iter(lst)
    head = Node(next(iterator))
    tail = head
    for element in iterator:
        tail.nxt = Node(element)
        tail = tail.nxt
    return head

def nth_from_last(lst, n):
    if n<0:
        raise ValueError("Negative values for n not allowed!")
    curr = lst
    result = lst
    i = 0
    while curr.nxt is not None:
        curr = curr.nxt
        if i >= n:
            result=result.nxt
        i += 1
    return result if i >= n else None

## test nth_from_last
my_list = make_linked_list(range(1,8))
for n in range(9):
    print(n, nth_from_last(my_list,n))



