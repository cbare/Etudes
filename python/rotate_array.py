"""
Rotate an array in place
"""
def rotate_to_new_list(l, k):
    """
    Return a new list that is the reult of rotating the original list
    to the right by k steps.
    """
    if l is None:
        return None
    if len(l) <= 1:
        return l

    n = len(l)
    k = k % n
    result = [None] * n
    for i in range(0,n):
        j = (i + k) % n
        result[j] = l[i]

    return result

def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def rotate(l, k):
    """
    Rotate a list of n elements in-place to the right by k steps.
    :example: with n = 7 and k = 3, the array [1,2,3,4,5,6,7] is rotated to [5,6,7,1,2,3,4]
    """
    if l is not None and len(l) > 1:
        n = len(l)
        g = gcd(n,k)
        ## for each group of indices that are congruent mod n, of which
        ## there are g, rotate that group
        for i0 in range(0,g):
            i = i0
            a = l[i]
            while True:
                j = (i + k) % n
                b = l[j]
                l[j] = a
                a = b
                i = j
                if i == i0:
                    break

l = [1,2,3,4,5,6]
k = 3
rotate(l,k)
print(l)

assert l == [4,5,6,1,2,3]

l = [1,2,3,4,5,6,7]
k = 3
rotate(l,k)
print(l)

assert l == [5,6,7,1,2,3,4]

l = list(range(1,37))
k = 15
rotate(l,k)
print(l)

assert l == rotate_to_new_list(list(range(1,37)), k)


def rotate_pearl(l, k):
    """
    Solution from Chapter 2 of Jon Bentley's Prpgramming Pearls
    """

    def _reverse(l,i,j):
        for d in range(0, (j-i)//2):
            a = i + d
            b = j - d - 1
            l[a],l[b] = l[b],l[a]

    _reverse(l, 0, len(l))
    _reverse(l, 0, k)
    _reverse(l, k, len(l))

l = list(range(1,37))
k = 15
rotate_pearl(l,k)
print(l)

assert l == rotate_to_new_list(list(range(1,37)), k)

