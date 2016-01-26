"""
debugging related routines.
debug can be activated by exporting
JIMN_DEBUG to a ':' separated list of modules to trace.
"""
import os


def __parse_debugged_modules():
    """
    parse JIMN_DEBUG env var to extract list of modules to be debugged.
    called at start.
    """
    debugged_modules = {}
    debugged_modules_string = os.environ.get("JIMN_DEBUG")
    if debugged_modules_string is not None:
        modules = debugged_modules_string.split(":")
        for module in modules:
            debugged_modules[module] = 1
    return debugged_modules


def is_module_debugged(module_name):
    """
    returns true if given module name is being traced
    """
    return module_name in DEBUGGED_MODULES


def add_module_to_debug(module_name):
    """
    activate all debug logs for given module.
    """
    DEBUGGED_MODULES[module_name] = 1


DEBUGGED_MODULES = __parse_debugged_modules()
