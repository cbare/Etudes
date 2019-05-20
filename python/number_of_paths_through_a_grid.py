"""
In a city laid out as a perfect grid, we want to walk from point
A at some intersection to point B at another intersection. The
problem is to figure out how many ways are there to get from A to
B without going out of your way.

The distances from A to B along each dimension of the grid are
given by x and y.
"""
from functools import lru_cache
from functools import reduce
from fractions import Fraction
import operator


@lru_cache(maxsize=1024)
def npaths(x, y):
    """
    Count paths recursively. Memoizing makes this efficient.
    """

    if x>0 and y>0:
        return npaths(x-1, y) + npaths(x, y-1)
    if x>0:
        return npaths(x-1, y)
    if y>0:
        return npaths(x, y-1)
    return 1


def npaths_dp(x,y):
    """
    Count number of paths by constructing Pascal's triangle,
    which is dynamic programming.
    """

    ## We'll fill in each position in the grid with the number of ways
    ## to get from the start to that position.
    grid = [[None for j in range(y+1)] for i in range(x+1)]

    ## The grid will look something like this:
    ## 1-1-1-1- ...
    ## | | | |
    ## 1-2-3-4- ...
    ## | | | |
    ## 1-3-6-10-...
    ## ...which is just Pascal's Triangle.

    ## along the edges, there's only 1 path
    for i in range(x+1):
        grid[i][0] = 1
    for j in range(y+1):
        grid[0][j] = 1

    ## any position in the grid is the sum of the two positions
    ## to the left and up from the current one.
    for i in range(1, x+1):
        for j in range(1, y+1):
            grid[i][j] = grid[i-1][j] + grid[i][j-1]

    ## print out the grid, just for laughs
    # for r in grid:
    #     print(r)

    ## return the resulting count
    return grid[x][y]


def prod(factors):
    """
    return the product of a sequence of factors
    """
    return reduce(operator.mul, factors, 1)

def n_choose_k(n,k):
    """
        n!
    ----------
     k!(n-k)!
    """
    return int(prod(Fraction(n-i,i+1) for i in range(k)))

def npaths_direct(x,y):
    """
    Entries in Pascal's triangle can be computed directly
    by n-choose-k
    """
    return n_choose_k(x+y, y)


def tests(number_of_paths):
    assert number_of_paths(0, 0) == 1
    assert number_of_paths(1, 0) == 1
    assert number_of_paths(0, 1) == 1
    assert number_of_paths(1, 1) == 2
    assert number_of_paths(2, 1) == 3
    assert number_of_paths(2, 2) == 6
    assert number_of_paths(3, 1) == 4
    assert number_of_paths(3, 2) == 10
    assert number_of_paths(3, 3) == 20
    assert number_of_paths(9, 4) == 715
    assert number_of_paths(6, 5) == 462
    assert number_of_paths(7, 6) == 1716
    assert number_of_paths(10, 4) == 1001
    assert number_of_paths(8, 5) == 1287
    assert number_of_paths(9, 6) == 5005


## Test eacb implementation
for f in (npaths, npaths_dp, npaths_direct):
    tests(f)


print("number_of_paths(9,6) =", npaths_direct(9, 6))
print("number_of_paths(15,15) =", npaths_direct(15, 15))

