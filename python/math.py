"""
Toy implementations of mathematical functions
"""

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
