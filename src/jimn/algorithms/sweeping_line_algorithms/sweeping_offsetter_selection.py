from jimn.algorithms.sweeping_line_algorithms import sweeping_line_algorithm
from jimn.point import point
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost


class sweeping_offsetter_selection(sweeping_line_algorithm):
    def __init__(self, paths):
        self.kept_paths = []
        super().__init__(paths)
        if __debug__:
            if is_module_debugged(__name__):
                print("kept paths")
                tycat(self.paths, self.kept_paths)

    def handle_new_paths(self, new_paths):
        for p in new_paths:
            up_to_new_path_winding = self.winding_number(p)
            if p.is_vertical():
                after_new_path_winding = \
                    up_to_new_path_winding + _path_orientation(p)
            else:
                after_new_path_winding = \
                    up_to_new_path_winding + _path_winding_number(p)
            # if windings are of opposite sign then this path is a limit
            # between outside and inside
            # other possibility is vertical path already inside
            if up_to_new_path_winding * after_new_path_winding < 0:
                self.kept_paths.append(p)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("keeping segment",
                              up_to_new_path_winding,
                              after_new_path_winding)
                        tycat(self.paths, self.kept_paths, p)
            else:
                if __debug__:
                    if is_module_debugged(__name__):
                        print("not keeping segment",
                              up_to_new_path_winding,
                              after_new_path_winding)
                        tycat(self.paths, self.kept_paths, p)

    def winding_number(self, limit_path):
        winding_number = 0.5
        above_paths = []
        for p in self.current_paths[0]:
            try:
                if p.is_above(limit_path):
                    above_paths.append(p)
            except:
                print("failed to compute above paths for", limit_path, p)
                tycat(self.paths, limit_path, p)
                raise
        if __debug__:
            if is_module_debugged(__name__):
                print("above paths")
                tycat(self.paths, limit_path, above_paths)
        for p in above_paths:
            winding_number += _path_winding_number(p)
        return winding_number


def _path_winding_number(p):
    points = p.get_endpoints()
    d = points[1] - points[0]
    w = d.cross_product(point([0, 1]))
    if is_almost(w, 0):
        return 0
    if w > 0:
        return 1
    else:
        return -1


def _path_orientation(p):
    points = p.get_endpoints()
    if points[0] < points[1]:
        return 1
    else:
        return -1


def select_offseted_paths(paths):
    s = sweeping_offsetter_selection(paths)
    return s.kept_paths
