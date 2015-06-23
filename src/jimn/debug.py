import os

debugging_initialized = False
debugged_modules = {}


def init_debugging():
    global debugged_modules
    debugged_modules_string = os.environ.get("JIMN_DEBUG")
    if debugged_modules_string is not None:
        modules_list = debugged_modules_string.split(":")
        for m in modules_list:
            debugged_modules[m] = 1


def is_module_debugged(module_name):
    global debugging_initialized
    if not debugging_initialized:
        init_debugging()
        debugging_initialized = True
    return module_name in debugged_modules
