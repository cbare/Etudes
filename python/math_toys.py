"""
Toy implementations of mathematical functions
"""

def fibonacci(n: int) -> int:
    """
    Iteratively compute fibonacci of n
    """
    if n==0: return 0
    if n==1: return 1
    fp2 = 0
    fp1 = 1
    for _ in range(1,n):
        f = fp1 + fp2
        fp2 = fp1
        fp1 = f
    return f

def test_fibonacci():
    assert fibonacci(1) == 1
    assert fibonacci(2) == 1
    assert fibonacci(3) == 2
    assert fibonacci(8) == 21

    print('  n |      fibonacci(n)')
    print('----+-----------------------')
    for i in range(100):
        print(f'{i:3} | {fibonacci(i):21}')



def sqrt(x: float, epsilon: float = 1e-6) -> float:
    """
    Square root
    """
    a = 0
    b = x
    r = x
    xp = r*r

    while abs(x-xp) > epsilon:
        r = (a+b)/2
        xp = r*r
        if xp < x:
            a = r
        else:
            b = r

    return r

def test_sqrt():
    print('sqrt(63)≈', sqrt(63))
    print('sqrt(64)≈', sqrt(64))
    print('sqrt(65)≈', sqrt(65))

    print('sqrt(15128)≈', sqrt(15128))
    print('sqrt(15129)≈', sqrt(15129))
    print('sqrt(15130)≈', sqrt(15130))


if __name__ == "__main__":
    test_sqrt()
    test_fibonacci()
