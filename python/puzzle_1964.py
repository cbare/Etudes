"""
A few years ago when my kid was in 4th grade or so, they gave out a challenge.
Given the number 1964 as a starting point, can you make all the counting
numbers from 1-100 as an arithmetic expression using only those digits in
order?

As far as I know, the allowed operations were: +, -, *, /, !, √ (square root),
| (absolute value), and raising to a power. For example:
- 1 = 1 ^ 964
- 2 = 1 - 9 + 6 + 4 or |(1-9) * (6/(4!))|
- 3 = 1 * (9 / (6 / √4 )) or √(19 - 6 - 4)
- etc.

Most numbers can be reached in many ways. But, can all be reached? My kiddo
and I (OK, mostly me) geeked out on this on paper, but wouldn't it be quicker
to let the computer do all the work?

Below is an attempt to find a solution for all 1-100 by casting the problem
as a search in the space of possible expressions. Starting with a set of
tuples containing the integers represented by all partitions of the string
"1964" we generate neighbors by adding operations. When we have a complete
expression, we evaluate it and store the resulting solution.

We could keep adding operators forever, but that would take too long, so we
limit the search depth to 8 or 9 operations.
"""
import itertools
import math
from collections import defaultdict
from numbers import Number


start = [
    (1,9,6,4),
    (19,6,4),
    (1,96,4),
    (1,9,64),
    (1,964),
    (19,64),
    (196,4),
    (1964,),
]

u_ops = "-√!|"
b_ops = "+-*/^"

def is_square(x):
    if isinstance(x, int) and x >= 0:
        return isqrt(x) ** 2 == x
    return False

def is_num(a):
    return isinstance(a, Number)

def is_unary(a):
    return isinstance(a, tuple) and len(a) == 2 and a[0] in u_ops

def is_binary(a):
    return isinstance(a, tuple) and len(a) == 3 and a[0] in b_ops

def is_complete(a):
    return is_num(a) or (isinstance(a, tuple) and len(a)==1)

def get_op(a):
    return a[0]

def eval_s(s):
    if is_num(s):
        return s
    elif is_unary(s):
        op, arg = s
        a = eval_s(arg)
        if op == '-':
            return a * -1
        elif op == '√':
            return math.isqrt(a)
        elif op == '!':
            return math.factorial(a)
        elif op == '|':
            return abs(a)
        else:
            raise ValueError(f"Unknown operator: {op}")
    elif is_binary(s):
        op, arg_a, arg_b = s
        a, b = eval_s(arg_a), eval_s(arg_b)
        if op == '+':
            return a + b
        elif op == '-':
            return a-b
        elif op == '*':
            return a*b
        elif op == '/':
            q,r = divmod(a,b)
            if r != 0:
                raise ValueError(f"Integral division only: {s}")
            return q
        elif op == '^':
            return a**b
        else:
            raise ValueError(f"Unknown operator: {op}")
    else:
        raise ValueError(f"Can't eval: {s}")

def pretty(s):
    if is_num(s):
        return str(s)
    elif is_unary(s):
        op, arg = s
        pretty_arg = pretty(arg)
        parens = not is_num(arg)
        if op == '-':
            return f"-({pretty_arg})" if parens else f"-{pretty_arg}" 
        elif op == '√':
            return f"√({pretty_arg})" if parens else f"√{pretty_arg}"
        elif op == '!':
            return f"({pretty_arg})!" if parens else f"{pretty_arg}!"
        elif op == '|':
            return f"|{pretty_arg}|"
        else:
            raise ValueError(f"Unknown operator: {op}")
    elif is_binary(s):
        op, arg_a, arg_b = s
        pretty_a, pretty_b = pretty(arg_a), pretty(arg_b)
        if op == '+':
            return f"({pretty_a} + {pretty_b})"
        elif op == '-':
            return f"({pretty_a} - {pretty_b})"
        elif op == '*':
            return f"({pretty_a} * {pretty_b})"
        elif op == '/':
            return f"({pretty_a} / {pretty_b})"
        elif op == '^':
            return f"({pretty_a} ^ {pretty_b})"
        else:
            raise ValueError(f"Unknown operator: {op}")
    else:
        raise ValueError(f"Can't eval: {s}")


def gen_unary_ops(state):
    for i in range(len(state)):
        pre, a, post = state[0:i], state[i], state[i+1:]

        if is_num(a) or is_binary(a) or (is_unary(a) and get_op(a) not in "-|"):
            yield pre + ( ('-', a), ) + post

        va = eval_s(a)
        if va > 3 and is_square(va):
            yield pre + ( ('√', a), ) + post

        if 2 < va < 7:
            yield pre + ( ('!', a), ) + post

        if va < 0 and (is_unary(a) and get_op(a) not in "-|"):
            yield pre + ( ('|', a), ) + post

def gen_binary_ops(state):
    for i in range(len(state)-1):
        pre, a, b, post = state[0:i], state[i], state[i+1], state[i+2:]
        va, vb = eval_s(a), eval_s(b)
        ops = (
            "+-*"
            + ("/" if vb != 0 and va % vb == 0 else "")
            + ("^" if 1 <= abs(va) < 100 and (0 < vb < 16) else "")
        )
        for op in ops:
            yield pre + ( (op, a, b), ) + post

def gen_neighbors(state):
    return itertools.chain(
        gen_unary_ops(state),
        gen_binary_ops(state),
    )


def dfs(start, depth=8, solutions=None):
    q = [(0,start)]
    visited = set()
    solutions = defaultdict(list) if solutions is None else solutions

    while q:
        d, state = q.pop()

        if d < depth:
            for neighbor in gen_neighbors(state):
                q.append((d+1, neighbor))
                if is_complete(neighbor):
                    val = eval_s(neighbor[0])
                    if 0 < val < 101:
                        solutions[val].append(neighbor)

    return solutions


solutions = defaultdict(list)
for s in start:
    print(s)
    dfs(s, solutions=solutions)
    print(len(solutions))


missing = [x for x in range(101) if x not in (solutions.keys())]

# sols = dfs(start[0], depth=8)


start1983 = [
  (1,9,8,3),
  (19,8,3),
  (1,98,3),
  (1,9,83),
  (1,983),
  (19,83),
  (198,3),
  (1983,),
]

solutions = defaultdict(list)
for s in start1983:
    print(s)
    dfs(s, solutions=solutions)
    print(len(solutions))


def string_partitions(s):
    """
    Maps a string of digits to a list of partitions as integers, for example:
    "1964" -> [(1,9,6,4), (1,9,64), (1,96,4), (1,964), (19,6,4), (19,64), (196,4), (1964)]

    The idea is that for each possible prefix, we recursively generate a partitioning of
    the suffix. For each partition of the suffix, we get one partition by concatenating
    the prefix and suffix partition.
    """
    if len(s) == 0:
        return [()]
    if len(s) == 1:
        return [(int(s),)]
    return [
        (int(s[:i]),) + part
        for i in range(1, len(s)+1)
            for part in string_partitions(s[i:])
    ]
