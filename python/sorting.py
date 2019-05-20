"""
Sorting exercises
"""
import random


def is_sorted(x):
    """
    Test whether a list is sorted
    """
    return all(a <= b for a,b in zip(x[0:-1], x[1:]))


def bubble_sort(x):
    """
    An inefficient sorting algorithm, in place
    """
    swapped = True
    last = len(x)-1
    while swapped:
        swapped = False
        for i in range(0, last):
            if x[i] > x[i+1]:
                x[i+1], x[i] = x[i], x[i+1]
                swapped = True
    return x


def merge_sort(x):
    """
    Merge sort, not super memory efficient
    """

    # a list of length 0 or 1 is already sorted
    if len(x) < 2:
        return x

    # split in half
    p = len(x)//2

    # sort the halfs
    l, r = merge_sort(x[:p]), merge_sort(x[p:])

    # merge the sorted halfs
    i = j = 0
    result = []
    while i < len(l) and j < len(r):
        if l[i] <= r[j]:
            result.append(l[i])
            i += 1
        else:
            result.append(r[j])
            j += 1

    return result + l[i:] + r[j:]


n = 10000
x = [random.randint(1,20) for _ in range(n)]
assert is_sorted(merge_sort(x))
