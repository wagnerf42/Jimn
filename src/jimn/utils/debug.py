import os


"""
debugging related routines.
debug can be activated by exporting
JIMN_DEBUG to a ':' separated list of modules to trace
"""


def parse_debugged_modules():
    debugged_modules = {}
    debugged_modules_string = os.environ.get("JIMN_DEBUG")
    if debugged_modules_string is not None:
        modules_list = debugged_modules_string.split(":")
        for m in modules_list:
            debugged_modules[m] = 1
    return debugged_modules


def is_module_debugged(module_name):
    """
    returns true if given module name is being traced
    """
    return module_name in debugged_modules


def add_module_to_debug(module_name):
    global debugged_modules
    debugged_modules[module_name] = 1


debugged_modules = parse_debugged_modules()
