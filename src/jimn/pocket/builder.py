from jimn.pocket import pocket
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.utils.debug import is_module_debugged
from collections import defaultdict

"""
takes a set of paths and walk on them, rebuilding larger connected structures
"""
area_limit = 10**-10 # TODO: what is the right value ??


class pockets_builder:
    def __init__(self, paths, reversed_paths):
        # initialize various structs to hold info
        self.points = []
        self.pockets = []
        self.previous_point = None
        self.current_point = None
        self.current_path = None
        self.reversed_paths = reversed_paths

        # start
        self.paths = paths
        if self.reversed_paths:
            self.add_reversed_paths()
        self.marked_paths = {}  # we go only once through each (oriented) path

        # compute structures to easily enter and leave points
        # we need to quickly find neighbours for any point
        self.hash_points()
        self.sort_neighbours_by_angle()

    def add_reversed_paths(self):
        reversed_paths = []
        for p in self.paths:
            reversed_path = p.reverse()
            reversed_paths.append(reversed_path)
        self.paths.extend(reversed_paths)

    def tycat(self, *others):
        tycat(self.pockets, self.points, self.current_path, self.paths,
              list(self.marked_paths.values()), *others)

    def hash_points(self):
        """computes for each point the list of neighbouring points"""
        self.points_neighbours = defaultdict(list)
        for p in self.paths:
            start, end = p.get_endpoints()
            self.points_neighbours[start].append(p)
            if not self.reversed_paths:
                self.points_neighbours[end].append(p)

    def sort_neighbours_by_angle(self):
        """sorts all neighbours so that we can easily turn around a point"""
        for p, neighbour_paths in self.points_neighbours.items():
            sorted_neighbours = sorted(
                neighbour_paths,
                key=lambda n: p.angle_with(n.get_endpoint_not(p)),
                reverse=True
            )
            self.points_neighbours[p] = sorted_neighbours

    def build_pockets(self):
        for s in self.paths:
            if s in self.marked_paths:
                continue  # skip paths already used
            try:
                p = self.build_pocket(s)
            except:
                print("failed building pocket")
                tycat_set_svg_dimensions(1024, 768)
                print(*self.points)
                self.polygons = []
                self.tycat()
                raise

            try:
                if not p.is_oriented_clockwise() and not p.of_reversed_arcs():
                    # discard outer edge
                    self.pockets.append(p)
                    if __debug__:
                        if is_module_debugged(__name__):
                            print("added pocket")
                            self.tycat()
            except:
                continue
        return self.pockets

    def find_next_path(self, current_point, previous_point):
        neighbours = self.points_neighbours[current_point]
        length = len(neighbours)
        for i, p in enumerate(neighbours):
            if p.get_endpoint_not(current_point) == previous_point:
                index = i
                break
        else:
            print("previous point not leading here")
            self.tycat(current_point, previous_point)
            raise Exception("previous point not leading here")

        # now, find next index ; it's tricky
        # we need to loop increasing index
        # each incoming path cancels an outgoing path
        # find first outgoing path not cancelled
        to_skip = 0
        for i in range(length-1):
            current_index = (index + i + 1) % length
            current_path = neighbours[current_index]
            if current_path.get_endpoint(0) == current_point:
                # we leave
                if to_skip == 0:
                    return current_path
                else:
                    to_skip -= 1
            else:
                # we arrive
                to_skip += 1
        raise Exception("cannot leave")

    def build_pocket(self, start_path):
        self.paths = []

        self.start_point, self.current_point = start_path.get_endpoints()
        self.paths.append(start_path)
        self.marked_paths[start_path] = start_path

        self.previous_point = self.start_point

        while self.current_point != self.start_point:
            # find where to go
            next_path = self.find_next_path(self.current_point, self.previous_point)
            if next_path.reverse() == self.paths[-1]:
                raise Exception("path going back")
            self.paths.append(next_path)
            self.marked_paths[next_path] = next_path
            # continue moving
            self.previous_point = self.current_point
            self.current_point = next_path.get_endpoint(1)

        return pocket(self.paths)


def build_pockets(paths, reverse_paths=True):
    """
    turns a set of paths into a set of pockets.
    works by following edges.
    pre-requisite: no paths intersect other than at endpoints.
    """
    builder = pockets_builder(paths, reverse_paths)
    return builder.build_pockets()

def build_polygons(paths):
    builder = pockets_builder(paths, True)
    pockets = builder.build_pockets()
    polygons = []
    for p in pockets:
        poly = p.to_polygon()
        if abs(poly.area()) > area_limit:
            poly.remove_useless_points()
            polygons.append(poly)
    return polygons
