"""
This is a bit of Python code with type annotations.

To type check this code with [mypy](http://mypy-lang.org/) like this:

mypy mypy/type_check_me.py
"""
from typing import List, TypeVar, Optional, Sequence

def gcd(a: int, b: int) -> int:
    """
    Greatest common devisor
    """
    while b:
        a, b = b, a % b
    return a

print('gcd(24, 42) = ', gcd(24, 42))

assert gcd(24, 42) == 6

# I slept on the 'z' key. This doesn't type check
# wut = gcd('zzz', 42)

# A typed collection
def caps(strings: List[str]) -> List[str]:
    return [s.upper() for s in strings]

curses = ['dangit', 'shucks', 'fooey', 'flapdoodle', 'asdf']
print('caps(curses) = ', caps(curses))

# A list with a type in it can't be capitalized
things = ['dangit', (1,2,3)]
# caps(things)

# Parameterized types and Optional
T = TypeVar('T')
def first(seq: Sequence[T]) -> Optional[T]:
    return seq[0] if seq else None

print('first(curses) = ', first(curses))
print('first(caps(curses)) = ', first(caps(curses)))
print('first(curses) = ', first([]))

def twice(a: Optional[str]) -> str:
    return a + ', ' + a if a else 'um, um'

print('twice(first(caps(curses))) = ', twice(first(caps(curses))))

assert first(caps(curses)) == 'DANGIT'
assert twice(first(caps(curses))) == 'DANGIT, DANGIT'

def first_or_die(seq: Sequence[T]) -> T:
    return seq[0]

# type-checks, but results at run-time in
# 'IndexError: list index out of range'
first_or_die(caps([]))
