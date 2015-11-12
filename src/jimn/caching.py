from copy import copy

"""
cache system for methods.
prefix each method whose result is cached by @cache
and any method invalidating all cached results by @invalidate_cache.
RESULTS RETURNED ARE THE SAME : so treat them as READ ONLY
"""

def cached(f):
    def helper(obj, *args):
        if not hasattr(obj, "object_cache"):
            obj.object_cache = {}

        method_name = str(f)
        if method_name in obj.object_cache:
            result = obj.object_cache[method_name]
        else:
            result = f(obj, *args)
            obj.object_cache[method_name] = result

        # return copy(result)
        return result
    return helper


def invalidate_cache(f):
    def helper(obj, *args):
        obj.object_cache = {}
        return f(obj, *args)
    return helper
