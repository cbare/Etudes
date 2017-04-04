import random
from stats import *

epsilon = 0.00001

def test_mean():
    assert mean([-11,11,-27,27,-1234,1234]) == 0
    assert mean([1,5,10,3,4]) == (1+5+10+3+4)/5.0
    assert mean([1.1,2.2,3.3,4.4]) == (1.1+2.2+3.3+4.4)/4.0
    assert mean(range(1,100)) == 50
    assert mean(range(1,101)) == 50.5, mean(range(1,101))

def test_median():
    assert median([1]) == 1
    assert median([1,3]) == 2
    assert median([1,2,3]) == 2
    assert median([3,2,1,3]) == 2.5

def test_mode():
    assert mode([1,2,3,4,5,6,7,8,9,2,3,2,3,2,2,2,6]) == 2

def test_weighted_mean():
    assert weighted_mean([1,2,3],[10,5,10]) == 2.0
    assert weighted_mean([98,93,89],[20,30,50])== 92.0
    v = [random.gauss(0,1) for i in range(100)]
    w1 = [3 for i in range(100)]
    w2 = [4 for i in range(100)]
    assert abs(weighted_mean(v, w1) - weighted_mean(v, w2)) < epsilon

def test_rank():
    assert rank([100,50,20]) == [3,2,1]
    assert rank([7,2,4,3,7,1,9]) == [5,2,4,3,5,1,6]

def test_sd():
    assert sd([1,1,1,1,1,1,1]) == 0.0
    assert sd([9,3,9,3]) == 3.0

    ## This test has ~3.3e-11 probability of failing.
    median_variance = mean(sd([random.gauss(0,3) for i in range(1000)]) for j in range(20))
    #print('median_variance=', median_variance)
    assert abs(median_variance - 3.0) < 0.1, median_variance

