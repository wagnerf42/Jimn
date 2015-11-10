import os


"""
debugging related routines.
debug can be activated by exporting
JIMN_DEBUG to a ':' separated list of modules to trace
"""


def is_module_debugged(module_name):
    """
    returns true if given module name is being traced
    """
    debugged_modules_string = os.environ.get("JIMN_DEBUG")
    debugged_modules = {}
    if debugged_modules_string is not None:
        modules_list = debugged_modules_string.split(":")
        for m in modules_list:
            debugged_modules[m] = 1
    return module_name in debugged_modules
