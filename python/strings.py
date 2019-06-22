"""
String algorithms
"""

def balanced_parens(s: str) -> bool:
    open = 0
    for c in s:
        if c=='(': open += 1
        if c==')':
            if open > 0:
               open -= 1
            else:
                return False
    return open==0


assert balanced_parens('')
assert balanced_parens('()')
assert balanced_parens('((()))')
assert balanced_parens('((()()()))')
assert balanced_parens('((()()()))()(())(()())')
assert not balanced_parens('(()')
assert not balanced_parens('((())))')
assert not balanced_parens('((()())')
assert not balanced_parens('())(()')


def longest_valid_parens(s: str) -> int:
    """
    return the length of the longest run of valid nested parens.

    Given a string containing just the characters '(' and ')', find the length
    of the longest well-formed substring.
    """
    seeds = [(i,i+1) for i in range(len(s)-1) if s[i:i+2]=='()']

    grew = True
    while grew or merged:

        grew = 0
        merged = 0

        # grow
        for i in range(len(seeds)):
            a,b = seeds[i] 
            if a>0 and b+1<len(s) and s[a-1]=='(' and s[b+1]==')':
                grew += 1
                seeds[i] = (a-1, b+1)

        # merge
        new_seeds = []
        s0 = seeds[0]
        for s1 in seeds[1:]:
            if s0[1]+1==s1[0]:
                merged += 1
                s0 = (s0[0], s1[1])
            else:
                new_seeds.append(s0)
                s0 = s1
        new_seeds.append(s0)
        seeds = new_seeds

    return max(b-a+1 for a,b in seeds)
