from jimn.precision import segment_limit
import copy


class elementary_path:
    def __init__(self, points):
        self.endpoints = points
        if __debug__:
            assert self.squared_length() > segment_limit, "very small path"

    def squared_length(self):
        """squared distance between endpoints"""
        return self.endpoints[0].squared_distance_to(self.endpoints[1])

    def reverse(self):
        copied_path = copy.copy(self)
        copied_path.endpoints = list(reversed(self.endpoints))
        return copied_path

    def has_extremity(self, intermediate_point):
        for p in self.endpoints:
            if p == intermediate_point:
                return True
            else:
                assert not p.is_almost(intermediate_point), "precision pb"
        return False

    def get_endpoint(self, index):
        return self.endpoints[index]

    def get_endpoints(self):
        return self.endpoints

    def sort_endpoints(self):
        copied_path = copy.copy(self)
        copied_path.endpoints = sorted(self.endpoints)
        return copied_path

    def is_sorted(self):
        sorted_self = self.sort_endpoints()
        return sorted_self.endpoints == self.endpoints

    def middle_point(self):
        """returns point at same distance of endpoints (not necessarily on path)"""
        p1, p2 = self.endpoints
        return p1 + (p2-p1) / 2

    def dimension(self):
        return self.endpoints[0].dimension()
