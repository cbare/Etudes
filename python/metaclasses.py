"""
Metaclasses and how to use them.

A class is a special callable object whose job is to construct objects
of a specific form. Similarly, a metaclass is a callable whose job is to
construct classes of a specific form.

For example, SQL Alchemy provides a metaclass that creates mappings between
database entities and Python objects and machinery to sync between them. As
a user, you define classes and their inheritence hierarchy to represent your
data model. Like the "abstract factory" design pattern, a metaclass can
construct a family of related classes.

See: "So you want to be a Python expert?" by James Powell
"""

class BaseMeta(type):
    def __new__(cls, name, bases, body):
        print(f"BaseMeta.__new__: {cls.__name__}, {name}")
        return super().__new__(cls, name, bases, body)

class Base(metaclass=BaseMeta):
    def __new__(cls, a):
        x = super().__new__(cls)
        print(f"Base.__new__({cls}) {x}")
        # false, because init hasn't happened yet
        print(f"{hasattr(x, 'a')=}")
        print(f"{hasattr(x, 'b')=}")
        print(f"{hasattr(x, 'bar')=}")
        return x
    def __init__(self, a):
        print(f"Base.__init__({a})")
        self.a = a
    def __init_subclass__(self, *args, **kwargs):
        print("__init_subclass__", args, kwargs)
        print(f"{hasattr(self, 'a')=}")
        print(f"{hasattr(self, 'bar')=}")
        return super().__init_subclass__(*args, **kwargs)
    def foo(self):
        print("foo!")
        return "foo"

class Derived(Base):
    def __init__(self, b):
        super().__init__(b**2)
        print(f"Derived.__init__({b})")
        self.b = b
    def bar(self):
        print(f"bar {self.a} {self.b}!")
        return self.foo()

if __name__ == "__main__":
    derived = Derived(123)
    print(f"{derived.bar()=}")
