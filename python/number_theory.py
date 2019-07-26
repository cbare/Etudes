"""
Number theory functions.
"""
import sys

from functools import reduce
from math import sqrt
from operator import mul

import pygraphviz as pgv


def prod(seq):
    return reduce(mul, seq, 1)


def is_prime(n):
    if n < 2 or n%2==0:
        return n==2
    for m in range(3,int(sqrt(n))+1,2):
        if n%m==0:
            return False
    return True


def even(n):
    return n%2 == 0


def odd(n):
    return n%2 == 1


def factor(n):
    """
    Factor an integer n returning a list of prime factors
    """
    f = 2
    fs = iter(range(3, int(sqrt(n))+1, 2))

    factors = []
    r = n
    try:
        while r > 1:
            while r%f==0:
                r = r//f
                factors.append(f)
            f = next(fs)
    except StopIteration:
        if r > 1:
            factors.append(r)
    return factors


def test_factor():
    assert factor(100) == [2,2,5,5]
    assert factor(23)  == [23]
    assert factor(871) == [13,67]
    assert factor(40)  == [2, 2, 2, 5]
    assert factor(2*3*5*7*11*13*17*19*23*29*31) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]


def collatz_sequence(n):
    """
    The Collatz sequence for n is a generated by iterating:

    a_n = (1/2) * a_n-1    if a_n-1 is even
    a_n = 3*a_n-1 + 1      if a_n-1 is odd

    ...a sequence which is conjectured to always wind up at 1.
    """
    s = []
    x = n
    while x>1:
        s.append(x)
        if x % 2 == 0:
            x = x//2
        else:
            x = 3*x + 1
    return s


def test_collatz_sequence():
    for n in range(1,100):
        print(collatz_sequence(n))

    print(max(len(collatz_sequence(n)) for n in range(1000)))


def collatz_graph(n, filename=None):
    """
    Use pygraphviz and Graphviz to visualize Collatz sequences for numbers
    up to n.
    """
    g = pgv.AGraph()
    g.add_node(1, fillcolor='#19a8ff30', style='filled')
    if n < 1: return g

    for x in range(2, n+1):
        a = x
        while a>1:
            if a % 2 == 0:
                b = a//2
                g.add_node(a, fillcolor='#19a8ff30', style='filled')
                g.add_edge(a,b, color='#19a8ff', dir='forward')
            else:
                b = 3*a + 1
                g.add_node(a, fillcolor='#e3423430', style='filled')
                g.add_edge(a,b, color='#e34234', dir='forward')
            a = b

    filename = filename or f'collatz_graph_{n}.svg'

    g.layout(prog='dot')
    g.draw(filename)


collatz_graph(1000)