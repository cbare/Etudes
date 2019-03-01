"""
Project Euler: Problem 15 
Lattice Paths
"""
import numpy as np

def lattice_paths(n, m):
    grid = np.ones([n,m], dtype=np.dtype('int64'))
    grid[0,0] = 0

    for i in range(1,n):
        for j in range(1,m):
            grid[i,j] = grid[i-1,j] + grid[i,j-1]

    #print(grid)

    return grid[-1,-1]

if __name__ == '__main__':
    print(lattice_paths(21, 21))
