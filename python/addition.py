"""
Add strings
"""

def _get_sign(a):
    return (-1, a[1:]) if a.startswith('-') else (1, a)

def _is_greater(a,b):
    if len(a)==len(b):
        return a>=b
    else:
        return len(a)>len(b)


def add(a:str, b:str) -> str:

    n = max(len(a), len(b))

    print(f'{a} + {b}')

    sign_a, a = _get_sign(a)
    sign_b, b = _get_sign(b)

    a,b,sign_a,sign_b = (a,b,sign_a,sign_b) \
        if _is_greater(a,b) else (b,a,sign_b,sign_a)

    carry = 0
    result = []
    for i in range(1, n+1):
        d = int(a[-i]) if i<=len(a) else 0
        e = int(b[-i]) if i<=len(b) else 0
        s = (d + e + carry) if sign_a==sign_b else (d - e + carry)
        if s >= 10:
            carry = 1
            s -= 10
        elif s < 0:
            carry = -1
            s += 10
        else:
            carry = 0
        result.append(str(s))

    if carry > 0:
        result.append(str(carry))

    return ('-' if sign_a==-1 else '') + ''.join(reversed(result))


def test():

    def _add(a,b):
        s = add(a,b)
        ok = 'ok' if int(s) == int(a) + int(b) else 'fail'
        print(f'{a} + {b} = {s}    {ok}')

    _add('1', '1')
    _add('999', '1')
    _add('123', '1234')
    _add('5001', '999')
    _add('56789', '56789')

    _add('11', '-3')
    _add('-3', '11')
    _add('-11', '3')
    _add('3', '-11')
    _add('56789', '-56789')
    _add('56789', '-56790')
