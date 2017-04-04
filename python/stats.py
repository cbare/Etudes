"""
Stats 101

Based on the 10 Days of Statistics Tutorial
see: https://www.hackerrank.com/domains/tutorials/10-days-of-statistics
"""
import random
from collections import Counter
from collections.abc import Sequence
from fractions import Fraction
from functools import reduce
from operator import mul, itemgetter
from math import sqrt, pi, factorial, exp, erf

def mean(a):
    if isinstance(a, Sequence):
        return sum(a)/float(len(a))
    else:
        s = n = 0
        for x in a:
            s += x
            n += 1
        return s/float(n)

def median(a, presorted=False):
    n = len(a)
    if not presorted:
        a = sorted(a)
    return (a[n//2]+a[n//2-1])/2.0 if len(a)%2==0 else a[n//2]

def mode(a):
    """
    Find the most commonly occurring item in the input list
    """
    index, _value = max(Counter(a).items(), key=itemgetter(1))
    return index

def weighted_mean(x,w):
    """
    Given equal length vectors of values and weights
    """
    return sum(xi*wi for xi,wi in zip(x,w)) / sum(w)

def rank(x):
    """
    Given a vector x, return an integer vector of the same length ranking the
    values of x, where equal values have equal rank.
    """
    r = 0
    x_prev = float('-inf')
    result = [None]*len(x)
    ## sort by value, i is the index of the value in the
    ## original unsorted list
    for i,xi in sorted(enumerate(x), key=itemgetter(1)):
        if xi>x_prev:
            r += 1
            x_prev = xi
        result[i] = r
    return result

def variance(a):
    mu = mean(a)
    return sum((x-mu)**2 for x in a)/len(a)

def sd(a):
    return sqrt(variance(a))

def covariance(x,y):
    mean_x = mean(x)
    mean_y = mean(y)
    return sum((xi-mean_x)*(yi-mean_y) for xi,yi in zip(x,y))/len(x)

def pearson_correlation(x,y):
    mean_x = mean(x)
    mean_y = mean(y)
    sd_x = sd(x)
    sd_y = sd(y)
    return sum( (xi-mean_x)*(yi-mean_y) for xi,yi in zip(x,y) )/(len(x)*sd_x*sd_y)

def spearman_unique_values(x,y):
    n = len(x)
    return 1 - 6*sum((xi-yi)**2 for xi,yi in zip(rank(x),rank(y)))/(n*(n**2-1))

def quartiles(a):
    a = sorted(a)
    n = len(a)
    q = n//4
    if n%4 == 0:
        q1 = mean(a[q-1],a[q])
        q2 = mean(a[n//2-1],a[n//2])
        q3 = mean(a[n-q-1],a[n-q])
    elif n%4 == 1:
        q1 = mean(a[q-1],a[q])
        q2 = a[n//2]
        q3 = mean(a[n-q-1],a[n-q])
    elif n%4 == 2:
        q1 = a[q]
        q2 = mean(a[n//2-1],a[n//2])
        q3 = a[n-q-1]
    elif n%4 == 3:
        q1 = a[q]
        q2 = a[n//2]
        q3 = a[n-q-1]
    return (q1,q2,q3)

def interquartile_range(a):
    q1,q2,q3 = quartiles(a)
    return q3-q1

def n_choose_k(n,k):
  return int( reduce(mul, (Fraction(n-i, i+1) for i in range(k)), 1) )

def binomial_pmf(x,n,p):
    return n_choose_k(n, x) * p**x * (1-p)**(n-x)

def geom_pmf(n,p):
    return (1-p)**(n-1)*p

def neg_binomial_pmf(x,n,p):
    return n_choose_k(n-1, x-1) * p**x * (1-p)**(n-x)

def poisson_pmf(lambda_, k):
    """
    poisson probability mass function
    """
    return lambda_**k * exp(-lambda_) / factorial(k)

def normal_pdf(mean, sd, x):
    """
    normal probability density function
    """
    return 1/(sd*sqrt(2*pi)) * exp(-((x-mu)**2)/(2*sd**2))

def normal_cdf(mean, sd, x, lower_tail=True):
    """
    normal cumulative density function

    If the time taken to assemble a car is normally distribution with a mean of
    20 hours and a standard deviation of 2 hours, the probability that a car can
    be assembled in less thand 19.5 hours is:

    normal_cdf(20, 2, 19.5)
    """
    return 1/2*(1+erf((x-mean)/(sd*sqrt(2)))) if lower_tail else 1 - 1/2*(1+erf((x-mean)/(sd*sqrt(2))))

def central_limit(mean, sd, n):
    return mean*n, sqrt(n)*2.0, n



def rnorm(n, mean=0.0, sd=1.0):
    return tuple(random.gauss(mean, sd) for i in range(n))

def runif(n, a=0.0, b=1.0):
    return tuple(random.uniform(a,b) for i in range(n))

def sequence(s,e,i):
    n = int((e-s)/i)
    return tuple(j*i+s for j in range(0,n+1))

