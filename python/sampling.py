"""
Sampling

Given a set of n items, sample k items such that every
item has an equal probability (k/n) of being sampled.
"""
import random
from math import sqrt

def mean(xs):
    n = len(xs)
    return sum(x/n for x in xs)

def sd(xs):
    """
    Compute population standard deviation
    """
    n = len(xs)
    xbar = mean(xs)
    var = sum(((x-xbar)**2)/n for x in xs)
    return sqrt(var)

def skewness(xs):
    """
    Skewness measures asymetry in the distribution of the given sample
    """
    n = len(xs)
    xbar = mean(xs)
    s = sd(xs)
    return sum(((x-xbar)**3)/n for x in xs) / (s**3)

def sample(items, k, replace=False):
    """
    Randomly sample k items with equal probability.
    """
    if not replace and k > len(items):
        raise ValueError("Sample must be no larger than population.")
    remaining = items
    sample = []
    for i in range(k):
        n = random.randint(0,len(remaining)-1)
        sample.append(remaining[n])
        if not replace:
            remaining = remaining[0:n] + remaining[n+1:]
    return sample

## Test on 1 sample
items = list(range(100))
k=25

s1 = sample(items, k)
print("s1=", s1)
assert items==list(range(100))
assert len(s1)==k
assert [si in items for si in s1]

## Test sampling with replacement
items = list(range(100))
k=200

s2 = sample(items, k, replace=True)
print("s2=", "[%s..." % ", ".join(str(x) for x in sorted(s2)[0:25]))
assert items==list(range(100))
assert len(s2)==k
assert [si in items for si in s2]
print("    mean of sample=", mean(s2))
print("      sd of sample=", sd(s2))
print("skewness of sample=", skewness(s2))

## Test a large number of samples
n = 1000
k = 25
samples = [sample(items, k) for i in range(n)]
cnts = {item:sum(item in smp for smp in samples) for item in items}

print("    mean count = ", mean(cnts.values()))
print("      sd count = ", sd(cnts.values()))
print("skewness count = ", skewness(cnts.values()))

expected_cnt = (k/len(items)*n)

assert abs(mean(cnts.values())-expected_cnt) < expected_cnt*0.1
assert sd(cnts.values()) < expected_cnt*0.5
assert abs(skewness(cnts.values())) < 1.0


def sample_stream(items, k):
    sample = []
    n = 0

    iterator = iter(items)

    while n < k:
        item = next(iterator)
        sample.append(item)
        n += 1

    ## The Wikipedia article on reservoir sampling
    ## has this nice algorithm
    for item in iterator:
        n += 1
        i = random.randint(0,(n-1))
        if i < k:
            sample[i] = item

    return sample

k=25

s3 = sample_stream(items, k)
print("s3=", s3)
assert items==list(range(100))
assert len(s3)==k
assert [si in items for si in s3]

## Test a large number of samples
n = 1000
samples = [sample_stream(items, k) for i in range(n)]
cnts = {item:sum(item in smp for smp in samples) for item in items}

print("    mean count = ", mean(cnts.values()))
print("      sd count = ", sd(cnts.values()))
print("skewness count = ", skewness(cnts.values()))

expected_cnt = (k/len(items)*n)

assert abs(mean(cnts.values())-expected_cnt) < expected_cnt*0.1
assert sd(cnts.values()) < expected_cnt*0.5
assert abs(skewness(cnts.values())) < 1.0

