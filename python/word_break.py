"""
Word break problem

Given an input string and a dictionary of words, segment
the input string into a space-separated sequence of
dictionary words if possible. For example, if the input
string is "applepie" and dictionary contains a standard
set of English words, then we would return the string
"apple pie" as output.

From: http://thenoisychannel.com/2011/08/08/retiring-a-great-interview-problem
See also: http://stackoverflow.com/questions/21273505/memoization-algorithm-time-complexity
"""

DICTIONARY = """bed bath and beyond bat bad on be an a at hand ton bean zz zzz zxzz""".split()

def is_word(s):
    return s in DICTIONARY

## could use from functools.lru_cache, but why not write our
## own memoize decorator
def memoize(f):
    cache = {}
    def wrapper(s):
        if s in cache:
            return cache[s]
        else:
            result = f(s)
            cache[s] = result
            return result
    return wrapper

## memoize calls to word_break to avoid repeated computations when we
## find different paths to the same substring
@memoize
def word_break(s):
    if len(s)==0:
        return []
    for i in reversed(range(len(s))):
        suffix = s[i:]
        if is_word(suffix):
            words = word_break(s[:i])
            if words is not None:
                return words + [suffix]
    return None

print(word_break("bedbathandbeyond"))
print(word_break("zzzxzzzzzxzzzzzzxzzzzzzxzzzzzxzzzzzxzzzxzzzxzz"))

