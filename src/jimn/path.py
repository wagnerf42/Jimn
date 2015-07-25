
"""
a path is a list of arcs or segments
"""

class path:
    def __init__(self, elementary_paths):
        self.elementary_paths = elementary_paths

    def fuse_with(self, starts):
        """
        take a set of cycles, hashed by their starting points.
        fuse them inside us.
        this is the last step of an eulerian path algorithm.
        """
        print("TODO")
