"""
Edit distance between two strings
"""
MATCH  = 0
DELETE = 1
INSERT = 2
OPS = {
    MATCH:'.',
    DELETE:'-',
    INSERT:'+'
}

def match(a,b):
    return 0 if a==b else 3

def insert(a):
    return 1

def delete(a):
    return 1

def index_min(values):
    return min(enumerate(values), key=lambda p: values.__getitem__(p[0]))

def edit_distance(s1, s2, match=match, insert=insert, delete=delete):
    print("----------"*6)
    print(s1,s2)
    n = len(s1)
    m = len(s2)

    ## allocate dynamic programming table
    d = [[None for j in range(m+1)] for i in range(n+1)]

    ## initialize rightmost column and bottom row
    d[-1][-1] = (0,0)
    for i in reversed(range(n)):
        d[i][-1] = (DELETE, delete(s1[i]) + d[i+1][-1][1])
    for j in reversed(range(m)):
        d[-1][j] = (INSERT, insert(s2[j]) + d[-1][j+1][1])

    for i in reversed(range(n)):
        for j in reversed(range(m)):

            opt = [
                match(s1[i],s2[j]) + d[i+1][j+1][1],
                delete(s1[i])      + d[i+1][j][1],
                insert(s2[j])      + d[i][j+1][1]]

            d[i][j] = index_min(opt)

    print("d=")
    for row in d:
        print([el[1] for el in row])
    for row in d:
        print('['+'  '.join(OPS[el[0]] for el in row) +']')

    return d[0][0][1], d


def diff(s1,s2):
    d, t = edit_distance(s1, s2)

    i=0
    j=0
    result = []
    while i<len(s1) or j<len(s2):
        op, cost = t[i][j]
        if op==MATCH:
            result.append(s1[i])
            i+=1
            j+=1
        elif op==DELETE:
            result.append('-'+s1[i])
            i+=1
        elif op==INSERT:
            result.append('+'+s2[j])
            j+=1
        else:
            raise RuntimeException("Invalid edit operation")

    return ''.join(result)


d,t = edit_distance("aa","a")
print("ed=", d, diff("aa","a"))
d,t = edit_distance("aa","aaaa")
print("ed=", d, diff("aa","aaaa"))
d,t = edit_distance("qabcqq","zaxczz")
print("ed=", d, diff("qabcqq","zaxczz"))


