"""
Imagine a strange 2-dimensional world whose main features are poles and water.

We are given a list of heights of the poles. We want to find a pair of poles
that can contain the maximum amount of 2-dimensional water. In the ascii-art
below, the correct solution is between poles 1 and 8 - an area of 7*7 = 49.

9
8   |         |
7   |~~~~~~~~~|~~~|   <-- max water level
6   | |       |   |
5   | |   |   |   |
4   | |   | | |   |
3   | |   | | | | |
2   | | | | | | | |
1 |_|_|_|_|_|_|_|_|
  0 1 2 3 4 5 6 7 8
"""

def max_area(hs):
    """
    Find the maximum area between 2 poles whose heights are given in
    the list hs.
    """
    i = 0
    j = len(hs)-1
    amax = (j-i) * min(hs[j], hs[i])

    while j-i > 1:
        if hs[i] <= hs[j]:
            i+=1
        else:
            j-=1
        a = (j-i) * min(hs[j], hs[i])
        if a > amax:
            amax = a

    return amax

a = [1,8,6,2,5,4,8,3,7]
print(max_area(a))
