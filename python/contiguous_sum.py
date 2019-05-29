from typing import List


def find_contiguous_sum(lst: List[int], target: int) -> bool:
    """
    Given a sequence of integers and an integer total target, return whether a
    contiguous sequence of integers sums up to target.
               [1, 3, 1, 4, 23], 8 # True (because 3 + 1 + 4 = 8)
               [1, 3, 1, 4, 23], 7 # False
    """
    
    for i in range(0,len(lst)):
        sum = 0
        for j in range(i, len(lst)):
            sum += lst[j]
            if sum==target:
                return True

    return False


def find_contiguous_positive_sum(lst: List[int], target: int) -> bool:
    """
    Given a sequence of POSITIVE integers and an integer total target, return whether a
    contiguous sequence of integers sums up to target.
               [1, 3, 1, 4, 23], 8 # True (because 3 + 1 + 4 = 8)
               [1, 3, 1, 4, 23], 7 # False
    """
    if len(lst)==0: return False

    s = 0
    i = 0
    j = 0

    while j < len(lst):
        while j < len(lst) and (i==j or s < target):
            s += lst[j]
            j += 1
        while i < j and s > target:
            s -= lst[i]
            i += 1
        if s == target:
            return True

    return False


def test1():
    assert find_contiguous_sum([1, 3, 1, 4, 23], 8) == True
    assert find_contiguous_sum([1, 3, 1, 4, 23], 7) == False
    assert find_contiguous_sum([], 7) == False
    assert find_contiguous_sum([1, 3, 2, 1], 7) == True

def test2():
    assert find_contiguous_sum([1, 5, -1, 4, 23], 8) == True

def test3():
    assert find_contiguous_positive_sum([1, 3, 1, 4, 23], 8) == True
    assert find_contiguous_positive_sum([1, 3, 1, 4, 23], 7) == False
    assert find_contiguous_positive_sum([1, 3, 1, 3, 23], 8) == True
    assert find_contiguous_positive_sum([1, 3, 2, 4, 23], 8) == False
    assert find_contiguous_positive_sum([1, 3, 2, 4, 23], 100) == False
    assert find_contiguous_positive_sum([1, 3, 2, 4, 23], 33) == True
    assert find_contiguous_positive_sum([5, 3, 2, 4, 23], 1) == False
    assert find_contiguous_positive_sum([5, 3, 2, 4, 23], 29) == True

if __name__ == "__main__":
    test1()
    test2()
    test3()
