"""
Walk a JSON-like data structure and yield the path to each leaf and its value.
"""
data = {
    "a": {
        "b": {
            "c": 1,
            "d": 2
        },
        "e": 3
    },
    "f": [4, 5, 6]
}


def walk_json(data, prefix=None):
    prefix = prefix or []
    if isinstance(data, dict):
        for key, value in data.items():
            yield from walk_json(value, prefix + [key])
    elif isinstance(data, list):
        for i, value in enumerate(data):
            yield from walk_json(value, prefix + [str(i)])
    else:
        yield ('.'.join(prefix), data)

for k,v in walk_json(data):
    print(k, v)
