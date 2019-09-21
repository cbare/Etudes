"""
Join parts of URLs by ensuring that parts are separated by exactly one slash.

Because, unfortunately urllib.parse.urljoin is not for joining URLs.
"""
from functools import reduce

def _join_slash(a, b):
    return a.rstrip('/') + '/' + b.lstrip('/')

def urljoin(*args):
   return reduce(_join_slash, args) if args else ''


if __name__ == '__main__':
    parts = ['https://foo-bar.quux.net', '/foo', 'bar', '/bat/', '/quux/']

    url = urljoin(*parts)
    print('url=', url)

    url = urljoin('https://quux.com/', '/path', 'to/file///', '//here/')
    print('url=', url)

    url = urljoin()
    print('url=', url)

    url = urljoin('//','beware', 'of/this///')
    print('url=', url)

    url = urljoin('/leading', 'and/', '/trailing/', 'slash/')
    print('url=', url)
