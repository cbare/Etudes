"""
Project Euler: Problem 14 
Longest Collatz sequence

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?
"""
memo = {}

def longest_collatz(m):
    longest = float('-Inf')
    for n in range(1,m):
        l = length_collatz(n)
        if l > longest:
            longest = l
            longest_n = n
    return longest_n, longest

def length_collatz(n):
    if n == 1:
        return 1
    elif n in memo:
        return memo[n]
    elif n % 2 == 0:
        l = 1 + length_collatz(n//2)
    else:
        l =  1 + length_collatz(3 * n + 1)
    memo[n] = l
    return l

def collatz_seq(n):
    if n == 1:
        return (1,)
    elif n % 2 == 0:
        return (n,) + collatz_seq(n//2)
    else:
        return (n,) + collatz_seq(3 * n + 1)

if __name__ == '__main__':
    m = 1000000
    longest_n, longest = longest_collatz(m)
    print(f'longest collatz sequence starting under {m} = {longest} starting with {longest_n}')
    print(collatz_seq(longest_n))
        
