"""
small utility to display example code.
use when writing documentation for a new module.

usage is easy:

def __tour():
    description = "..."
    example = ("...")
    tour("module_name", description, example)

if __name__ == "__main__":
    __tour()
"""

def tour(module, description, example):
    """
    display and execute example.
    """
    print("welcome to", module)
    print("\n", description, "\n")
    print("here are some examples for you:\n")
    print("\n****** CODE ******\n")
    print(example)
    print("\n****** RESULTS ******\n")
    exec(example)
