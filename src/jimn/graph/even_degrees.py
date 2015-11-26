from jimn.utils.debug import is_module_debugged
from jimn.graph.bellman_ford import bellman_ford
from jimn.displayable import tycat


def make_degrees_even(g):
    for v in g.get_vertices():
        if not v.even_degree():
            _augment_path(g, v)


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
