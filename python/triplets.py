"""
List all the ways in which three distinct positive integers have a product of 108.

This problem come from a notebook by Peter Norvig titled:
"The Languages of English, Math, and Programming"

https://github.com/norvig/pytudes/blob/main/ipynb/Triplets.ipynb
"""
from math import prod
from number_theory import factor

def partitions(seq, k):
    """
    Generator over all the ways to partition seq into k parts.
    """
    if len(seq) == 0:
        yield tuple(() for _ in range(k))
    else:
        for xs in partitions(seq[1:], k):
            for i in range(k):
                yield tuple(
                    (seq[0],)+x if j == i else x
                    for j, x in enumerate(xs)
                )

# Ideally, we'd generate these partitions without duplications,
# but I dunno how to do that.
#
# ([], [2], [2,3,3,3])
# ([], [3], [2,2,3,3])
# ([], [2,2], [3,3,3])
# ([], [2,3], [2,3,3])
# ([], [3,3], [2,2,3])
# ([2], [3], [2,3,3])
# ([2], [2,3], [3,3])
# ([3], [2,2], [3,3])

def is_distinct(xs):
    return len(set(xs)) == len(xs)


def find_triplets(n, k):
    """
    List all the ways in which three distinct positive integers have a product of n
    """
    pfs = factor(n)

    return sorted(set(
        tuple(sorted(prod(p) for p in ps))
        for ps in partitions(pfs, k=k)
        if is_distinct(ps)
    ))


n = 108
k = 3
triplets = find_triplets(n, k)
print(f"There are {len(triplets)} distinct triplets.\n")
for triplet in triplets:
    print(f"prod{triplet} = {prod(triplet)}")
