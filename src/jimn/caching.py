from copy import copy


def cached(f):
    def helper(obj):
        if not hasattr(obj, "object_cache"):
            obj.object_cache = {}

        method_name = str(f)
        if method_name in obj.object_cache:
            result = obj.object_cache[method_name]
        else:
            result = f(obj)
            obj.object_cache[method_name] = result

        return copy(result)
    return helper


def invalidate_cache(f):
    def helper(obj, *args):
        obj.object_cache = {}
        return f(obj, *args)
    return helper
