"""
Return a list of letter combinations by the mapping of digits to
letters on a telephone dial.
"""

CODE = {
    '0': [''],    # 0 encodes the empty string
    '1': [''],    # 1 encodes the empty string
    '2': 'abc',
    '3': 'def',
    '4': 'ghi',
    '5': 'jkl',
    '6': 'mno',
    '7': 'pqrs',
    '8': 'tuv',
    '9': 'wxyz',
}

def telephone(digits, prefix=''):
    """
    telephone returns a list of possible translations of a string of digits
    into a matching string of characters by the telephone keypad code.

    digits: str of digits
    prefix: str
    """
    if not digits:
        return [prefix]
    letters = CODE[digits[0]]

    result = []
    for letter in letters:
        result.extend(telephone(digits[1:], prefix+letter))

    return result

def test():
    print(telephone('23'))
    print(telephone('012'))
    print(telephone('79'))

