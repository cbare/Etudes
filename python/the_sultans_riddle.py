"""
The Sultan's Riddle

source: https://explainextended.com/2016/12/31/happy-new-year-8/

Once upon a time there was a Sultan who was looking for a vizier to help him
rule his country. It became known to him that among the multitudes of his loyal
subjects that populated his glorious empire, two were regarded as the most wise
and sharp in mind. Their names were Ali-ibn-Wali and Wali-ibn-Ali. The Sultan
summoned the men to his palace and ordered them to stand in front of him.

"It has come to my attention that you, Ali, and you, Wali, are the smartest men
of all the people of Faith. Is that right?", asked the Sultan, sipping his
sharbat. "We do know a thing or two of the beasts of the land and the fish of
the sea and stars of the sky, indeed, but your sheer wisdom, o Great Sultan,
outshines whatever puny bits of knowledge we might have and makes words coming
from our mouths sound like child's babbling", said the wise men, kneeling
before the Sultan (as they were truly wise and knew how to talk to a man of
high power).

"Good, good," said the Sultan with a sneer, "I see you are good with words but
are you as good with numbers? Let me test your knowledge."

"A diviner once came to my palace and revealed two numbers to me," continued
the Sultan, "one being my lucky number, and another one being my unlucky
number. Each of these numbers is more than one and less than a hundred. I
never tell these numbers to anyone as this could put the fate of my empire in
enemy's hands. But I am going to multiply those numbers and secretly tell their
product to you, Ali, and then I am going to add those numbers and secretly tell
their sum to you, Wali. If you are as wise as they say, you will have no
problem figuring out those numbers."

And then he ordered the men to approach him and kneel before him, and he
whispered the numbers to their ears.

Once Ali raised from his knees, he stood there for a moment, silently moving
his lips, and then said: "Unfortunately, о the Brightest One, I cannot tell you
those numbers."

"This is true," confirmed Wali, stroking his beard.

"Thank you, most esteemed Wali," said Ali, his face brightening with joy. "Now
I can tell those numbers."

"Thank you too, most esteemed Ali," answered Wali. "Now I can tell them too."

And they whispered the numbers to the ear of the astonished Sultan, and they
turned out right. And they both were appointed viziers.

Can you tell those numbers?
"""
from math import sqrt
from collections import Counter

def is_prime(n):
    if n < 2 or n%2==0:
        return n==2
    for m in range(3,int(sqrt(n))+1,2):
        if n%m==0:
            return False
    return True

def even(n):
    return n%2 == 0

def odd(n):
    return n%2 == 1

def factor(n):
    for m in range(2, int(sqrt(n))+1):
        if n%m==0:
            return [m] + factor(n//m)
    return [n]

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

def prod(seq):
    """
    return the product of all numbers in seq
    """
    p = 1
    for a in seq:
        p *= a
    return p

def pairs_of_factors(n):
    """
    Given a whole number, return the set of pairs of factors, for example,
    given 12, return {(2,6), (3,4)}
    """
    seq = factor(n)
    # indexes into seq
    i = set(range(len(seq)))
    # create pairs of subsets indexes into seq and their complements
    ps = [(ss, i-ss) for ss in powerset(i) if 0 in ss and ss<i]
    return frozenset(
        tuple(sorted((prod(seq[i] for i in a), prod(seq[i] for i in b))))
        for a, b in ps)

def is_sum_of_primes(n):
    return even(n) or is_prime(n - 2)

def has_single_eligible_pair(n):
    """
    Return True if n does not have a unique factorization into two factors
    between 1 and 100 and exactly 1 factorization cannot be expressed as the
    sum of two prime numbers.
    """
    pairs = [(a,b) for a,b in pairs_of_factors(n) if 1<a<100 and 1<b<100]
    num_eligible_pairs = sum(not is_sum_of_primes(a+b) for a,b in pairs)
    return len(pairs) > 1 and num_eligible_pairs==1

def partitions(n):
    """
    Generate pairs of whole numbers a,b that sum to n where a <= b and a > 1.
    """
    for a in range(2,n//2+1):
        yield a, n-a


# if Ali can't, at first, name the two numbers that means the product he is
# given does not have a unique factorization into two factors between 1 and 100.
# First, let's make a lookup table, so we can tell how many ways there are to
# form each possible product out of some pair of a,b between 1 and 100.
count_pairs_of_factors = Counter()
for a in range(2, 100):
    for b in range(a, 100):
        count_pairs_of_factors[a*b] += 1

# Wali can only confirm the Ali can't, at first, answer if all partitions
# of his sum yield pairs whose product doesn't have a unique factorization
possible_sums = Counter()
for a in range(2, 100):
    for b in range(a, 100):
        if all(count_pairs_of_factors[prod(p)] > 1 for p in partitions(a+b)):
            possible_sums[a+b] += 1



print(f'    a    b  a+b    a*b  factors')

for a in range(2, 100):
    for b in range(a, 100):
        # Ali can't, at first, name a and b.
        if count_pairs_of_factors[a*b] == 1:
            continue
        # Wali confirms that Ali can't name the numbers.
        if is_sum_of_primes(a+b):
            continue
        # Knowing that Wali can confirm gives Ali the additional
        # information that Wali's sum cannot be expressed as the sum of two
        # primes. If exactly one potential pair of factors sums to a number
        # with that property, Ali can now name the two numbers.
        if not has_single_eligible_pair(a*b):
            continue
        # Wali now knows that Ali's product had one suitable factorization.
        # Looking at all possible partitions of his sum, he can check for
        # partitions whose products that have only one such factorization. If
        # he's lucky enough to be given a sum with exactly 1 such partition,
        # that's the Sultan's pair of numbers.
        eligible_partitions = tuple((n,m) for n,m in partitions(a+b)
                               if has_single_eligible_pair(n*m))
        if eligible_partitions != ((a,b),):
            continue

        print(f'{a:>5}{b:>5}{a+b:>5}{a*b:>7}  {factor(a*b)}')
