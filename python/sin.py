"""
Compute the sine function using the series:
    sin(x) = x - (x^3)/3! + (x^5)/5! - (x^7)/7! + ...

Let's try out some functional goodness by computing the sine function
using a series. The general strategy will be to create a generator for
terms in the series. The terms magnitude is (x^n)/n! where n is an odd
natural number, which we compute as a product of x/j as j goes from 1
to n, like so:

>>> n = 3
>>> prod(pi/j for j in range(1,n+1))          # doctest: +ELLIPSIS
5.1677...

The sign of the terms alternates, so let's make that happen like this:

>>> [(-1)**(i//2) for i in (1,3,5,7,9)]
[1, -1, 1, -1, 1]

We use the count generator from itertools to generate an indefinitely
long series of terms, knowing that the terms come in order of decreasing
magnitude. We can stop when the magnitude of the terms gets small enough
that we don't care.

>>> ', '.join('{:.4}'.format(t) for t in takewhile(lambda t: abs(t) > 0.001, _sin_terms(pi/2)))
'1.571, -0.646, 0.07969, -0.004682'

If we sum up these terms, we get an approximate value for the sine of the
input in radians.

>>> sin(pi/2)                                 # doctest: +ELLIPSIS
1.00...
>>> sin(pi/4)                                 # doctest: +ELLIPSIS
0.707...
>>> sin(5*pi/4)                               # doctest: +ELLIPSIS
-0.707...

"""
from itertools import count, takewhile
from functools import reduce
from operator import mul
from fractions import Fraction
from math import pi

def prod(factors):
    """
    Given an iterable of factors, returns their product
    """
    return reduce(mul, factors, 1)

def _sin_terms(x):
    """
    generates terms of the series x - (1/3!)x^3 + (1/5!)x^5 - (1/7!)x^7 + ...
    """
    return (((-1)**(i//2) * prod(x/j for j in range(1,i+1))) for i in count(1,2))

def sin(radians, epsilon=1e-20):
    """
    sums terms of the series x - (1/3!)x^3 + (1/5!)x^5 - (1/7!)x^7 + ...
    up to the point where the magnitude of the term is less than epsilon.
    """
    return sum(takewhile(lambda t: abs(t) > epsilon, _sin_terms(radians)))
