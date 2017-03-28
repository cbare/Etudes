def powerset(a):
    """
    Return the set of all subsets of a.
    """
    if len(a) == 0:
        return set([frozenset()])
    accumulator = set()
    a = set(a)
    element = a.pop()
    for subset in powerset(a):
        accumulator.add(subset)
        accumulator.add(frozenset(set([element]) | subset))
    return accumulator

print(powerset("ab"))
print(powerset("abc"))
print(powerset("abcd"))



def powerset(seq):
    """
    Returns a generator over all the subsets of the given seq.
    http://www.technomancy.org/python/powerset-generator-python/
    """
    if len(seq) == 0:
        yield []
    else:
        for item in powerset(seq[1:]):
            yield [seq[0]]+item
            yield item


def pp(g):
    print('-'*30)
    for i in g:
        print(i)


pp(powerset([]))
pp(powerset(["a"]))
pp(powerset(["a", "b"]))
pp(powerset(["a", "b", "c"]))
pp(powerset(["a", "b", "c", "d"]))
