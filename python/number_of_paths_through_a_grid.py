"""
In a city laid out as a perfect grid, we want to walk from point
A at some intersection to point B at another intersection. The
problem is to figure out how many ways are there to get from A to
B without going out of your way.

The distances from A to B along each dimension of the grid are
given by x and y.
"""
def number_of_paths(x,y):
    ## We'll fill in each position in the grid with the number of ways
    ## to get from the start to that position.
    grid = [[None for j in range(y)] for i in range(x)]

    ## The start will look something like this:
    ## 1-1-1-1- ...
    ## | | | |
    ## 1-2-3-4- ...
    ## | | | |
    ## 1-3-6-10-...
    ## ...which is just Pascal's Triangle.

    ## along the edges of the grid, there's only 1
    for i in range(x):
        grid[i][0] = 1
    for j in range(y):
        grid[0][j] = 1

    ## any position in the grid is the sum of the two positions
    ## to the left and up from the current one.
    for i in range(1,x):
        for j in range(1,y):
            grid[i][j] = grid[i-1][j] + grid[i][j-1]

    ## print out the grid, just for laughs
    # for r in grid:
    #     print(r)

    ## return the resulting count
    return grid[x-1][y-1]


print("number_of_paths(9,6) =", number_of_paths(9,6))
assert number_of_paths(3,3) == 6
assert number_of_paths(9,6) == 1287
assert number_of_paths(10,5) == 715
assert number_of_paths(7,6) == 462
assert number_of_paths(8,7) == 1716
assert number_of_paths(11,5) == 1001


## Since the entries in Pascal's triangle can be computed
## directly by n-choose-k or:
##     n!
## ----------
##  k!(n-k)!
##

from functools import reduce
from fractions import Fraction
import operator

def prod(factors):
    return reduce(operator.mul, factors, 1)

def n_choose_k(n,k):
    return int(prod(Fraction(n-i,i+1) for i in range(k)))

def number_of_paths(x,y):
    return n_choose_k(x+y-2,y-1)

assert number_of_paths(3,3) == 6
assert number_of_paths(9,6) == 1287
assert number_of_paths(10,5) == 715
assert number_of_paths(7,6) == 462
assert number_of_paths(8,7) == 1716
assert number_of_paths(11,5) == 1001

x,y = 9,6
print(x,y,n_choose_k(x+y-2,y-1))





