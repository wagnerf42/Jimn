"""
implementation of betley ottmann intersection algorithm.
"""
from collections import defaultdict
from sortedcontainers import SortedSet, SortedList
from jimn.utils.debug import is_module_debugged
from jimn.tree.treap import Treap
from jimn.point import Point
from jimn.displayable import tycat
from jimn.bounding_box import BoundingBox
from jimn.utils.coordinates_hash import ROUNDER2D
from jimn.segment import Segment
from jimn.elementary_path import set_comparer



class Event:
    def __init__(self, )

class InclusionTreeBuilder:
    """
    this class builds a tree of polygons included in one another.
    it works through a sweeping line algorithm.
    also identifies each as a hole or a polygon.
    """
    def __init__(self, polygons):

        # the algorithm works in O(n) (times sorted container's costs) in this way:
        # we have a SortedList of all currently crossed paths
        self.crossed_paths = SortedList()
        # for each polygon, a SortedList of all of its currently crossed paths
        self.polygons = defaultdict(SortedList)
        # when meeting a new polygon for the first time
        # we will insert it in the crossed_paths list ; we get it's top neighbour (smaller)
        # and get the corresponding polygon
        # now if we are contained inside it, we are its child
        # if we are not contained inside it, we are its brother
        # to figure out whether we are inside or not, we look at #paths smaller than us
        # in the nieghbour polygon's SortedList

        self.events = self._create_events()
        self.current_point = None
        self.tree = None
        self.identified_polygons = 0

        for event in self.events:
            self.execute_event(event)
            if self.identified_polygons == len(polygons):
                return  # no need to finish the sweep once everyone is identified

    def execute_event(event):
        if event_angle >= 0:
            # starting path
            self.current_point = event_point
            self.start_path(event_path)
        else:
            # ending path
            self.end_path(event_path)
            self.current_point = event_point

    def start_path(self, path):
        """
        handles incoming path
        """
        self.crossed_paths.add(path)
        raise Exception("TODO")

    def end_path(self, path):
        """
        handles ending path
        """
        raise Exception("TODO")
        return self

def build_inclusion_tree(polygons):
    """
    turn a set of polygons hashed by height into a polygon tree.
    """
    builder = InclusionTreeBuilder(polygons)
    return builder.tree
