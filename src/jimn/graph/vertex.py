from jimn.point import point


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = [] #degree is very low so this is ok
        self.frontier_edges = []

    def to_point(self):
        return point(self.coordinates)

    def get_edges(self):
        return self.edges

    def get_bounding_box(self):
        box = super(vertex, self).get_bounding_box()
        for e in self.edges:
            edge_box = e.get_bounding_box()
            box.update(edge_box)
        return box

    def save_svg_content(self, display, color):
        super(vertex, self).save_svg_content(display, color)
        for e in self.edges:
            p = e.get_path()
            count = e.get_multiplicity()
            if p.is_sorted():
                p.save_svg_content(display, display.get_color(count+7))

    def remove_any_edge(self):
        """
        removes and returns one edge
        (decreases multiplicity in case of multi-edges)
        """
        e = self.edges[-1]
        if e.get_multiplicity() == 1:
            self.edges.pop()
        else:
            e.change_multiplicity(-1)
        return e

    def remove_edge_to(self, destination):
        """
        removes one edge going to vertex destination
        (decreases multiplicity in case of multi-edges)
        """
        for i, e in enumerate(self.edges):
            if e.get_endpoint(1) == destination:
            #if e.get_endpoint(1).get_id() == destination.get_id():
                self._delete_edge(i)
                return
        else:
            raise Exception("edge not found")

    def edge_index(self, e):
        """
        returns index for given edge or None if not here
        """
        for i, f in enumerate(self.edges):
            if f.is_same(e):
                return i
        return None


    def add_edge(self, e, frontier_edge):
        """
        adds given edge.
        if already here, increases edge multiplicity
        """
        #assert e.get_endpoint(0).get_id() == self.vertex_id
        e_index = self.edge_index(e)
        if e_index is not None:
            self.edges[e_index].change_multiplicity(1)
        else:
            self.edges.append(e)

        if frontier_edge:
            self.frontier_edges.append(e)

    def _delete_edge(self, i):
        multiplicity = self.edges[i].change_multiplicity(-1)
        if multiplicity == 0:
            del self.edges[i]

    def degree(self):
        return sum([e.get_multiplicity() for e in self.edges])

    def even_degree(self):
        return self.degree() % 2 == 0

    def has_frontier_edge(self, searched_e):
        """
        returns true if one of our frontier edges
        is given edge
        """
        for e in self.frontier_edges:
            if e.is_same(searched_e):
                return True
        return False

    def has_frontier_edges_on_different_sides_of(self, y):
        """
        returns true if our frontier edges
        are on different sides of horizontal line
        at given y
        """
        aboves = [e.is_above_y(y) for e in self.frontier_edges]
        return (aboves[0] != aboves[1])

    def other_frontier_edge(self, avoided_e):
        """
        returns edge in frontier edges
        which is not given edge
        """
        for e in self.frontier_edges:
            if not e.is_same(avoided_e):
                return e
        raise Exception("no different edge")

    def get_non_horizontal_frontier_edge(self):
        """
        returns edge in frontier edges
        which is not horizontal
        """
        for e in self.frontier_edges:
            if not e.is_horizontal():
                return e
        raise Exception("only horizontal edges")

    def find_first_neighbor_not(self, neighbor):
        for e in self.edges:
            if neighbor is None or e.get_endpoint(1) != neighbor:
                return e
        raise Exception("only one neighbor")

    def __str__(self):
        return str(id(self)) + "\n\t" \
                             + "\n\t".join([str(e) for e in self.get_edges()])
