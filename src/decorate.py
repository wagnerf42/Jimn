#!/usr/bin/env python3
from jimn.caching import cached, invalidate_cache
from time import sleep

class testobj:
    def __init__(self):
        self.test = 1

    @invalidate_cache
    def increment(self, first=1, second=2):
        self.test += 1 + first + second

    @cached
    def even(self):
        sleep(2)
        return self.test % 2 == 0


a = testobj()
a.increment()
b = testobj()
print(a.even())
print(b.even())
print(b.even())
print(b.test)
b.increment(3, 5)
print(b.test)
print(b.even())
print(b.even())
