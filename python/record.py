"""
A Record is like a JSON-like object. It's properties can be
accessed using dot notation or like a dictionary.

Given a JSON-like structure, wrap it in a record
>>> props = {
...   'name':'Henry',
...   'age':4,
...   'address':{
...     'street':'1914 3rd Ave N',
...     'city':'Seattle',
...     'state':'WA',
...     'zip':'98109'},
...   'likes':[
...     {'name':'excavator',   'category':'trucks'},
...     {'name':'backhoe',     'category':'trucks'},
...     {'name':'frontloader', 'category':'trucks'},
...     {'name':'Kwazii',      'category':'octonauts'}]}
>>> henry = Record(props)

Now access it's properties with friendly object notations:
>>> henry.name
'Henry'

>>> henry.address.city
'Seattle'

Or, use dictionary-style notation, if you like:
>>> henry['age']
4

Adding new properties works usual:
>>> henry.lunch = 'macaroni and cheese'
>>> henry['lunch']
'macaroni and cheese'

>>> 'lunch' in henry
True

>>> henry['doesnt_exist']
Traceback (most recent call last):
   ...
KeyError: 'doesnt_exist'

>>> henry.no_existe
Traceback (most recent call last):
   ...
AttributeError: 'Record' object has no attribute 'no_existe'

Inspired by chapter 19 Dynamic Attributes and Properties of Luciano
Ramalho's Fluent Python.
"""
import collections.abc as abc

class Record(abc.MutableMapping):
    """
    A Record is like a JSON-like object. It's properties can be
    accessed using dot notation or like a dictionary.
    """

    def __new__(cls, arg=None, **kwargs):
        if kwargs and arg:
            raise ValueError('Initialize a Record either by passing in a map, a list or a set of keyword arguements')
        elif isinstance(arg, abc.Mapping) or kwargs:
            return super().__new__(cls)
        elif isinstance(arg, abc.MutableSequence):
            return [cls(item) for item in arg]
        else:
            return arg

    def __init__(self, arg=None, **kwargs):
        if arg:
            for k,v in arg.items():
                self.__dict__[k] = self.__class__(v)
        if kwargs:
            for k,v in kwargs.items():
                self.__dict__[k] = self.__class__(v)

    def __repr__(self,):
        return 'Record(' + ', '.join('{!s}={!r}'.format(k,v)
                                    for k,v in self.__dict__.items()) + ')'

    def __setitem__(self, k, v):
        self.__dict__[k]=v

    def __delitem__(self, k):
        del self.__dict__[k]

    def __getitem__(self, k):
        return self.__dict__[k]

    def __len__(self):
        return len(self.__dict__)

    def __contains__(self, key):
        return key in self.__dict__

    def __iter__(self):
        return iter(self.__dict__)

    def __getattr__(self, a):
        if hasattr(self.__dict__, a):
            print('why does this get called?')
            return getattr(self.__dict__, a)
        else:
            raise AttributeError('{!r} object has no attribute {!r}'.format(self.__class__.__name__, a))
