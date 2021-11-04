"""
Useful utility functions
"""
import os
import re
from collections.abc import Iterable


def flatten(xs):
    """
    https://stackoverflow.com/questions/2158395/flatten-an-irregular-list-of-lists
    """
    for x in xs:
        if isinstance(x, Iterable) and not isinstance(x, (str, bytes)):
            yield from flatten(x)
        else:
            yield x


def access(data, path, *args, default=None):
    """
    Access an element at a path in a json-like data structure. Return default
    if path does not exist.
    """
    try:
        for k in flatten((path, args)):
            data = data[k]
        return data
    except (KeyError, IndexError, TypeError):
        return default


def get_function_name(f):
    """
    Get name for either a function or a functools.partial.
    """
    try:
        return f.__name__
    except AttributeError:
        pass

    # this works for functools.partial objects
    try:
        return f.func.__name__
    except AttributeError:
        pass

    return type(f).__name__


def human_readable(n):
    """
    Print sizes in bytes in more human readable form.
    https://stackoverflow.com/a/1094933/199166
    """
    for unit in ['B','K','M','G','T','P','E','Z']:
        if abs(n) < 1024.0:
            return f"{n:6.1f}{unit}"
        n /= 1024.0
    return f"{n:6.1f}Y"


def ensure_dir_exists(path: str) -> str:
    """
    Make any path elements that don't already exist. Return the path
    or an empty string if path is None for use in os.path.join(...).
    """
    if path:
        if os.path.exists(path):
            if not os.path.isdir(path):
                raise ValueError(f"Expected \"{path}\" to be a directory.")
        else:
            os.makedirs(path)
    return path


def exactly_one_of(*args):
    return sum(bool(arg) for arg in args) == 1
