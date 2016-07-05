"""
take a set of paths and walk on them, rebuilding larger connected structures.
"""
from collections import defaultdict
from jimn.pocket import Pocket
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import SEGMENT_LIMIT, is_almost


class PocketsBuilder:
    """
    algorithm building pockets by following edges.
    """
    def __init__(self, paths, reversed_paths):
        # initialize various structs to hold info
        self.pockets = []
        self.reversed_paths = reversed_paths

        # start
        self.paths = paths
        if self.reversed_paths:
            self._add_reversed_paths()
        self.marked_paths = {}  # we go only once through each (oriented) path

        # compute structures to easily enter and leave points
        # we need to quickly find neighbours for any point
        self._hash_points()
        self._sort_neighbours_by_angle()

    def _add_reversed_paths(self):
        """
        paths can be used both ways.
        """
        reversed_paths = [p.reverse() for p in self.paths]
        self.paths.extend(reversed_paths)

    def _hash_points(self):
        """
        compute for each point the list of neighbouring points.
        """
        self.points_neighbours = defaultdict(list)
        for path in self.paths:
            start, end = path.endpoints
            self.points_neighbours[start].append(path)
            if not self.reversed_paths:
                self.points_neighbours[end].append(path)

    def _sort_neighbours_by_angle(self):
        """
        sort all neighbours so that we can easily turn around a point.
        """
        for point, neighbour_paths in self.points_neighbours.items():
            sorted_neighbours = sorted(
                neighbour_paths,
                key=lambda n, p=point: p.angle_with(n.get_endpoint_not(p)),
                reverse=True
            )
            self.points_neighbours[point] = sorted_neighbours

    def build_pockets(self):
        """
        run the algorith.
        """
        for start_path in self.paths:
            if start_path in self.marked_paths:
                continue  # skip paths already used
            try:
                pocket = self.build_pocket(start_path)
            except:
                print("failed building pocket")
                raise

            self.pockets.append(pocket)
            if __debug__:
                if is_module_debugged(__name__):
                    print("added pocket")
                    tycat(self.paths, pocket)

        return self.pockets

    def find_next_path(self, current_point, previous_point):
        """
        we came from previous point, and are now at current_point.
        return what is next point.
        """
        neighbours = self.points_neighbours[current_point]
        length = len(neighbours)
        for i, neighbour in enumerate(neighbours):
            if neighbour.get_endpoint_not(current_point) == previous_point:
                index = i
                break
        else:
            print("previous point not leading here")
            raise Exception("previous point not leading here")

        # now, find next index ; it's tricky
        # we need to loop increasing index
        # each incoming path cancels an outgoing path
        # find first outgoing path not cancelled
        to_skip = 0
        for i in range(length-1):
            tested_index = (index + i + 1) % length
            tested_path = neighbours[tested_index]
            if tested_path.endpoints[0] == current_point:
                # we leave
                if to_skip == 0:
                    return tested_path
                else:
                    to_skip -= 1
            else:
                # we arrive
                to_skip += 1

        print("we are at", current_point, "coming from", previous_point,
              "available paths are", [str(n) for n in neighbours])
        raise Exception("cannot leave")

    def build_pocket(self, start_path):
        """
        start with start_path and follow edge building the pocket.
        """
        current_path = []

        start_point, current_point = start_path.endpoints
        current_path.append(start_path)
        self.marked_paths[start_path] = start_path

        previous_point = start_point

        while current_point != start_point:
            # find where to go
            next_path = self.find_next_path(current_point, previous_point)
            if __debug__:
                if next_path.reverse() == current_path[-1]:
                    tycat(self.paths, current_path, next_path)
                    raise Exception("path going back")

            current_path.append(next_path)
            self.marked_paths[next_path] = next_path
            # continue moving
            previous_point = current_point
            current_point = next_path.endpoints[1]

        return Pocket(current_path)


def build_pockets(paths):
    """
    turns a set of paths into a set of pockets.
    works by following edges.
    pre-requisite: no paths intersect other than at endpoints.
    paths can be oriented if reverse_paths is set to false.
    """
    builder = PocketsBuilder(paths, False)
    return builder.build_pockets()


def build_polygons(paths):
    """
    follow all given paths, building polygons.
    """
    builder = PocketsBuilder(paths, True)
    pockets = builder.build_pockets()
    polygons = []
    for pocket in pockets:
        poly = pocket.to_polygon()
        # keep non-clockwise polygons, big enough
        try:
            clockwise = poly.is_oriented_clockwise()
        except:
            print("bad polygon: ", poly)
            tycat(paths, poly.points)
            raise
        if not clockwise and abs(poly.area()) > SEGMENT_LIMIT:
            simpler_poly = poly.remove_useless_points()
            if __debug__:
                if is_almost(poly.area(), 0):
                    tycat(simpler_poly, poly)
                    raise Exception("BADPOLYGON")
            polygons.append(simpler_poly)
    return polygons
