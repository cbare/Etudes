"""
We have walls of different heights. Such pictures are represented
by an array of integers, where the value at each index is the height
of the wall. Fig. 1 is represented with an array as:

[2,5,1,2,3,4,7,7,6].

Now imagine it rains. How much water is going to be accumulated in
puddles between walls? For example, if it rains the following puddle
will be formed:

        77 
        ||6
   5~~~~|||
   |~~~4|||
   |~~3||||
  2|~2|||||
  ||1||||||
  ---------

see: http://chrisdone.com/posts/twitter-problem-loeb
"""

def water(hs):

    ## How much water sits on top of column i
    def w_at_i(hs, i):
        mxl = max(hs[0:i]) if i > 0 else 0
        mxr = max(hs[i:]) if i+1 < len(hs) else 0
        return min(mxr,mxl) - hs[i] if mxl > hs[i] and mxr > hs[i] else 0

    return sum( w_at_i(hs, i) for i in range(len(hs)) )

## test cases
print(water([1,2,3,4,5,5,4,3,2,1]))
print(water([5,4,3,2,1,3,5,7,9]))
print(water([2,5,1,2,3,4,7,7,6]))
print(water([4,1,3,2,3,5,7,5,4]))
print(water([4,2,6,5,4,7,6,5,7,4]))

