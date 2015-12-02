from jimn.utils.debug import is_module_debugged
from jimn.graph.bellman_ford import bellman_ford
from jimn.displayable import tycat


def make_degrees_even(g):
    for v in g.get_vertices():
        if not v.even_degree():
            _augment_path(g, v)


def make_degrees_even_fast(g, milling_diameter):
    """
    fast approximation algorithm to obtain even degrees.
    loop on outer edge and duplicate edges between odd and even cut lines
    then loop on inner edges and duplicate edges with non valid degrees.
    bad cases appear when there is a spike between two milling levels
    or when we have even number of slices
    """
    def add_edges_in_slices(g, milling_diameter, slices_parity):
        """
        add edges for given parity
        """
        added_edges = []
        value = 0
        for e in g.frontier_edges():
            if not e.is_horizontal():
                s = e.slice_number(milling_diameter)
                if (s % 2) == slices_parity:
                    added_edges.append(e)
                    value += e.get_path().length()
                    g.add_direct_edge(e)

        for e in g.get_non_oriented_edges():
            vertices = e.get_endpoints()
            if (not vertices[0].even_degree()) and \
                    (not vertices[1].even_degree()):
                g.add_direct_edge(e)
                value += e.get_path().length()
                added_edges.append(e)

        return (value, added_edges)

    value, added_edges = add_edges_in_slices(g, milling_diameter, True)
    # cancel and try other parity
    for e in added_edges:
        e.remove()
    new_value, added_edges = add_edges_in_slices(g, milling_diameter, False)
    if new_value > value:
        # cancel again and revert to first parity
        for e in added_edges:
            e.remove()
        add_edges_in_slices(g, milling_diameter, True)


def _augment_path(g, v):
    # this is a very simple way to find the best augmenting path
    # it is in no way optimized
    # and has a complexity of O(n^2)
    distances, predecessors = bellman_ford(g, v)
    destination = _find_nearest_odd_vertex(g, v, distances)
    current_point = destination
    if __debug__:
        if is_module_debugged(__name__):
            added_edges = []
    while current_point != v:
        e = predecessors[current_point.id]
        g.add_direct_edge(e)
        if __debug__:
            if is_module_debugged(__name__):
                added_edges.append(e)
        previous_point = e.vertices[0]
        current_point = previous_point
    if __debug__:
        if is_module_debugged(__name__):
            print("new augmenting path")
            tycat(g, added_edges)
            print("graph is now")
            tycat(g)


def _find_nearest_odd_vertex(g, v, distances):
    current_distance = float("inf")
    for destination in g.get_vertices():
        distance = distances[destination.id]
        if (destination.id != v.id) and (current_distance > distance):
            if destination.degree() % 2:
                best_destination = destination
                current_distance = distance
    return best_destination
