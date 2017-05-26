"""
Use futures to waste CPU cycles in parallel.

example:
    python3 parallel.py -r -m 100 -n 1000000
"""
import click
import random
from concurrent import futures
from stats import mean, sd

def identity(x):
    return x

def from_a_hundred_to(n):
    return random.randint(100,n)

def sample_stats(n):
    """
    Compute mean and standard deviation on a bunch of 
    random numbers
    """
    sample = tuple(random.random() for i in range(n))
    return mean(sample), sd(sample)

def parallel_stats(m, n, ssf=None):
    """
    Return an iterator over completed futures containing
    mean and sd tuples
    """
    if not ssf:
        ssf = identity
    with futures.ProcessPoolExecutor() as executor:
        future_stats = [executor.submit(sample_stats, ssf(n)) for i in range(m)]
    return futures.as_completed(future_stats)

@click.command()
@click.option('-n', '--sample-size', type=int, default=100)
@click.option('-m', '--samples', type=int, default=10)
@click.option('-r', '--randomize/--not-randomized', default=False)
def main(samples=10, sample_size=100, randomize=False):
    """
    Demo concurrent.futures by computing a bunch of stats
    """
    ms = []
    sds = []
    ssf=from_a_hundred_to if randomize else identity
    print('\n    mean\t      sd')
    print('-'*40)
    for future in parallel_stats(samples, sample_size, ssf=ssf):
        sm, ssd = future.result()
        print(f'{sm:.6f}\t{ssd:.6f}')
        ms.append(sm)
        sds.append(ssd)

    print('-'*40)
    print('{:.6f}\t{:.6f}'.format(mean(ms), sd(ms)))

if __name__ == '__main__':
    main()
