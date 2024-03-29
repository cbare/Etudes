"""
3Sum

Given a list of integers, find all unique triplets in the list which sum to zero.

A variant of this problem is to find the elements a, b, and c in the list whose
sum is closest to a target sum.
"""
from typing import List
from collections import Counter



def threeSumCubic(xs: List[int]) -> List[List[int]]:
    """
    Brute force n^3 solution to 3sum problem
    """
    solutions = set()
    for i in range(len(xs)-2):
        for j in range(i+1, len(xs)-1):
            for k in range(j+1, len(xs)):
                if xs[i] + xs[j] + xs[k] == 0:
                    solutions.add(tuple(sorted((xs[i], xs[j], xs[k]))))
    return sorted(solutions)


def partitions(x):
    """
    Generate partitions of an integer x
    """
    sign = 1 if x >= 0 else -1
    n = abs(x)
    for i in range(0, n//2+1):
        yield sign*i, sign*(n-i)


def threeSumByPartition(xs: List[int]) -> List[List[int]]:
    """
    Solve 3sum by iterating over partitions of the additive inverse of each
    member of the list checking for the presence of a and b, the two parts
    of each partition.

    Takes 2 passes through the list, the first builds the counter and the
    second looks for solutions. It's O(n*m) where n is the length of the
    list and m is x//2+1 where x is the size of elements in the list.
    """
    c = Counter(xs)
    solutions = set()
    for x in xs:
        for a,b in partitions(-x):
            if x==0 and a==0 and b==0:
                if c[a]>2:
                    solutions.add(tuple(sorted((x,a,b))))
            elif a==b:
                if c[a]>1:
                    solutions.add(tuple(sorted((x,a,b))))
            else:
                if (a in c and b in c):
                    solutions.add(tuple(sorted((x,a,b))))

    return sorted(solutions)


def threeSumHash(xs: List[int]) -> List[List[int]]:
    """
    convert the n^3 algorithm to n^2 by use of a hash table
    """
    solutions = set()
    h = set()
    for i in range(1, len(xs)-1):
        h.add(xs[i-1])
        for j in range(i+1, len(xs)):
            a = xs[i]
            b = xs[j]
            c = (-a - b)
            if c in h:
                a1 = min((a, b, c))
                c1 = max((a, b, c))
                solutions.add((a1, -c1-a1, c1))
    return sorted(solutions)


def threeSum(xs: List[int]) -> List[List[int]]:
    """
    3sum using 3 pointers into a sorted array
    """
    ## if xs is too small, there are no solutions
    if len(xs) < 3: return []

    xs = sorted(xs)
    solutions = set()
    for i in range(len(xs)-2):
        j = i+1
        k=len(xs)-1
        while j<k:
            if xs[i]+xs[j]+xs[k]==0:

                ## found a solution
                solutions.add(tuple(sorted((xs[i], xs[j], xs[k]))))
                j += 1

                ## skip duplicate values
                while j < k and xs[j]==xs[j-1]:
                    j += 1
            elif xs[i]+xs[j]+xs[k] < 0:
                j += 1
            else:
                k -= 1

    return sorted(solutions)


def threeSumClosest(x: List[int], target: int) -> int:
    """
    Find the three elements of x whose sum is closest to the given target
    and return their sum.
    """
    if len(x) < 3:
        raise RuntimeError('input list must have at least 3 elements')

    x = sorted(x)
    soln = sum(x[0:3])

    ## n^2 nested loops
    for i in range(len(x)-2):
        for k in range(i+2, len(x)):

            ## binary search for the j between i and k that minimizes
            ## the difference between the 3sum and the target
            a = i
            b = k

            while b-a>1:
                j = a + (b-a)//2
                s = x[i] + x[j] + x[k]
                if s == target:
                    return target
                elif s < target:
                    a = j
                else:
                    b = j

                if abs(target-s) < abs(target-soln):
                    soln = s

    return soln


def fourSum(xs: List[int], target: int) -> List[List[int]]:
    xs = sorted(xs)
    solns = set()
    
    for h in range(0, len(xs)-3):
        for k in range(h+3, len(xs)):
            i = h+1
            j = k-1
            
            while j>i:
            
                s = xs[h] + xs[i] + xs[j] + xs[k]
                if s==target:
                    solns.add((xs[h], xs[i], xs[j], xs[k]))
                    i += 1
                    j -= 1

                elif s<target:
                    i+=1
                else:
                    j-=1
                
    return sorted(solns)



def test_threeSumCubic():
    assert threeSumCubic([0,0]) == []
    assert threeSumCubic([0,0,0]) == [(0,0,0)]
    assert threeSumCubic([3,0,-2,-1,1,2]) == [(-2,-1,3), (-2,0,2), (-1,0,1)]
    assert threeSumCubic([-1,0,1,2,-1,-4]) == [(-1,-1,2), (-1,0,1)]

def test_threeSumHash():
    assert threeSumHash([0,0]) == []
    assert threeSumHash([0,0,0]) == [(0,0,0)]
    assert threeSumHash([3,0,-2,-1,1,2]) == [(-2,-1,3), (-2,0,2), (-1,0,1)]
    assert threeSumHash([-1,0,1,2,-1,-4]) == [(-1,-1,2), (-1,0,1)]

def test_threeSumByPartition():
    assert threeSumByPartition([0,0]) == []
    assert threeSumByPartition([0,0,0]) == [(0,0,0)]
    assert threeSumByPartition([3,0,-2,-1,1,2]) == [(-2,-1,3), (-2,0,2), (-1,0,1)]
    assert threeSumByPartition([-1,0,1,2,-1,-4]) == [(-1,-1,2), (-1,0,1)]
    assert threeSumByPartition([-6, 1, 5, -7, 2]) == [(-7, 2, 5), (-6,1,5)]

def test_threeSum():
    assert threeSum([0,0]) == []
    assert threeSum([0,0,0]) == [(0,0,0)]
    assert threeSum([3,0,-2,-1,1,2]) == [(-2,-1,3), (-2,0,2), (-1,0,1)]
    assert threeSum([-1,0,1,2,-1,-4]) == [(-1,-1,2), (-1,0,1)]

def test_threeSumClosest():
    assert threeSumClosest([1,6,9,14,16,70], 81) == 80
    assert threeSumClosest([0,2,1,-3], 1) == 0
    assert threeSumClosest([-1,2,1,-4], 1) == 2

def test_fourSum():
    assert fourSum([-1,2,2,-5,0,-1,4], 3) == [(-5,2,2,4), (-1,0,2,2)]
    assert fourSum([1,0,-1,0,-2,2], 0) == [(-2,-1,1,2), (-2,0,0,2), (-1,0,0,1)]

if __name__ == "__main__":
    import types
    tests = [
        globals()[k] for k in globals().keys()
        if k.startswith("test_") and type(globals()[k]) is types.FunctionType
    ]
    for test in tests:
        print(f"{test.__name__:30}", end='')
        try:
            test()
            result = "ok"
        except AssertionError as ex:
            result = f"failed! {ex}"
        print(result)
