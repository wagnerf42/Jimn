from jimn.point import point
from collections import defaultdict


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = []
        self.multiplicities = defaultdict(int)
        self.edges_number = 0

    def get_bounding_box(self):
        box = super(vertex, self).get_bounding_box()
        for e in self.edges:
            edge_box = e.get_bounding_box()
            box.update(edge_box)
        return box

    def save_svg_content(self, display, color):
        super(vertex, self).save_svg_content(display, color)
        for e, count in self.multiplicities.items():
            e.save_svg_content(display, display.get_color(count+20))

    def get_edges(self):
        return self.edges

    def get_edge(self, index):
        return self.edges[index]

    def remove_any_edge(self):
        e = self.edges.pop()
        self.update_multiplicity(e, -1)
        return e

    def update_multiplicity(self, target_edge, diff_count):
        self.edges_number += diff_count
        assert self.edges_number >= 0
        self.multiplicities[target_edge] += diff_count
        if self.multiplicities[target_edge] == 0:
            del self.multiplicities[target_edge]

    def remove_edge_to(self, destination):
        """
        removes one edge going to destination
        """
        kept_edges = []
        removed_edge = False
        for edge in self.edges:
            if (not removed_edge) and (edge.get_endpoint(1) == destination):
                removed_edge = True
                self.update_multiplicity(edge, -1)
            else:
                kept_edges.append(edge)
        self.edges = kept_edges
        assert removed_edge

    def add_edge(self, edge):
        self.edges.append(edge)
        self.update_multiplicity(edge, 1)

    def delete_edge(self, edge):
        if edge in self.edges:
            self.edges.remove(edge)
            self.update_multiplicity(edge, -1)
        else:
            assert edge.reverse() in self.edges
            reversed_edge = edge.reverse()
            self.edges.remove(reversed_edge)
            self.update_multiplicity(reversed_edge, -1)

    def degree(self):
        return self.edges_number

    def even_degree(self):
        return self.edges_number % 2 == 0

    def has_edge(self, edge):
        """
        returns true if one of our first two edges
        is given edge
        """
        for e in self.edges[:2]:
            if e.is_same(edge):
                return True
        return False

    def has_edges_on_different_sides_of(self, y):
        """
        returns true if our first two edges
        are on different sides of horizontal line
        at given y
        """
        aboves = [e.is_above_y(y) for e in self.edges[:2]]
        return (aboves[0] != aboves[1])

    def other_edge(self, edge):
        """
        returns edge in first two edges
        which is not given edge
        """
        for e in self.edges[:2]:
            if not e.is_same(edge):
                return e
        raise Exception("no different edge")

    def get_non_horizontal_edge(self):
        """
        returns edge in first two edges
        which is not horizontal
        """
        for e in self.edges[:2]:
            if not e.is_horizontal():
                return e
        raise Exception("only horizontal edges")

    def find_first_neighbor_not(self, neighbor):
        for e in self.edges:
            if neighbor is None or e.get_endpoint(1) != neighbor:
                return e
        raise Exception("only one neighbor")

    def shrink_multiedges(self):
        """
        turns multiedges of odd multiplicities into
        one edge and those of even multiplicities into
        two edges.
        """
        for e, count in self.multiplicities.items():
            self.multiplicities[e] = 1 + ((count-1) % 2)
