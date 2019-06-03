"""
Fun with linked lists.
"""

class Node:
    """An element of a linked list"""
    def __init__(self, value, next=None):
        self.value = value
        self.next = next

    def __repr__(self):
        return str(self.value) + "," + (str(self.next) if self.next else '')

def make_linked_list(lst):
    """
    Turn a Python list (or any iterable) into a linked list
    """
    if len(lst)==0: return None
    iterator = iter(lst)
    head = Node(next(iterator))
    tail = head
    for element in iterator:
        tail.next = Node(element)
        tail = tail.next
    return head

def nth_from_last(lst, n):
    """
    Given a linked list and n, find the nth element from the
    end of the list.
    """
    if n<0:
        raise ValueError("Negative values for n not allowed!")
    curr = lst
    result = lst
    i = 0
    while curr.next is not None:
        curr = curr.next
        if i >= n:
            result=result.next
        i += 1
    return result if i >= n else None

## test nth_from_last
my_list = make_linked_list(range(1,8))
for n in range(9):
    print(n, nth_from_last(my_list,n))


def swap_pairs(head: Node) -> Node:
    """
    Given a linked list, swap every two adjacent nodes and return the modified list
    """
    if head is None or head.next is None:
        return head
    else:
        a = head
        b = head.next
        c = b.next
        b.next = a
        a.next = swap_pairs(c)
        return b

print(swap_pairs(my_list))



def reverseKGroup(head: Node, k: int) -> Node:
    """
    Given a linked list, reverse groups of length k
    so reverseKGroup(make_linked_list([1,2,3,4,5]), k=2) == make_linked_list([2,1,4,3,5])
    """
    i = k
    current = head
    prev = None
    nxt = None

    while i > 0 and current:
        nxt = current.next
        if prev:
            current.next = prev
        prev = current
        current = nxt
        i -= 1

    if head:
        head.next = reverseKGroup(nxt, k) if nxt else None

    return prev

l = make_linked_list([])
print(reverseKGroup(l, k=2))

l = make_linked_list([1])
print(reverseKGroup(l, k=2))

l = make_linked_list([1,2])
print(reverseKGroup(l, k=2))

l = make_linked_list([1,2,3,4,5])
print(reverseKGroup(l, k=2))

l = make_linked_list([1,2,3,4,5,6,7,8,9])
print(reverseKGroup(l, k=3))
