"""
cache system for methods.
prefix each method whose result is cached by @cached or @cached_args
and any method invalidating all cached results by @invalidate_cache.
RESULTS RETURNED ARE THE SAME : so treat them as READ ONLY.
be very careful with this system. it is very easy to modify objects
indirectly, thus invalidating the cache inadvertantly.
"""


def cached_last_arg(method):
    """
    cache method, but only last argument.
    ONLY USABLE ON A SINGLE METHOD.
    """
    def helper(obj, *args):
        """
        wrapper for cached method.
        """
        if not hasattr(obj, "last_asked") or obj.last_asked != args[0]:
            obj.last = method(obj, args[0])
        obj.last_asked = args[0]
        return obj.last

    return helper


def cached_args(method):
    """
    mark method as to be cached.
    arguments ARE taken into account AND MUST BE HASHABLE.
    """
    def helper(obj, *args):
        """
        wrapper for cached_args method.
        """
        if not hasattr(obj, "object_args_cache"):
            obj.object_args_cache = {}

        arguments_tuple = tuple(a for a in args)

        method_name = str(method)
        if method_name not in obj.object_args_cache:
            obj.object_args_cache[method_name] = {}

        if arguments_tuple in obj.object_args_cache[method_name]:
            result = obj.object_args_cache[method_name][arguments_tuple]
        else:
            result = method(obj, *args)
            obj.object_args_cache[method_name][arguments_tuple] = result

        return result
    return helper


def cached(method):
    """
    mark method as to be cached.
    arguments are not taken into account so results returned are the same
    regardless of arguments.
    """
    def helper(obj, *args):
        """
        wrapper for cached method.
        """
        if not hasattr(obj, "object_cache"):
            obj.object_cache = {}

        method_name = str(method)
        if method_name in obj.object_cache:
            result = obj.object_cache[method_name]
        else:
            result = method(obj, *args)
            obj.object_cache[method_name] = result

        return result
    return helper


def invalidate_cache(method):
    """
    destroys all results in cache.
    use this decoration on methods modifying object's content.
    """
    def helper(obj, *args):
        """
        wrapper for cached invalidation.
        """
        obj.object_cache = {}
        obj.object_args_cache = {}
        return method(obj, *args)
    return helper
