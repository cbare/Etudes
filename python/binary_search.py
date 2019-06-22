
import random


def binary_search(l, target):
    """
    given a sorted list 'l' and a target value, return the position of target
    in the list or None if not found.
    """
    i = 0
    j = len(l)

    while j>i:
        m = (j-i)//2 + i
        if l[m] == target:
            return m
        elif l[m] < target:
            i = m+1
        else:
            j = m

    return None


def test():
    letters = "abcdefghijklmnopqrstuvwxyz"
    a = sorted(random.choices(letters, k=20))
    print(a)
    target = 'a'
    i = binary_search(a, target)
    print(f'found {target} at position {i}')
