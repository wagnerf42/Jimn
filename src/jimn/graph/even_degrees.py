"""
algorithms adding (multi) edges to a graph to obtain
even degrees on all vertices.
"""
from jimn.utils.debug import is_module_debugged
from jimn.graph.bellman_ford import bellman_ford
from jimn.displayable import tycat


def make_degrees_even(graph):
    """
    slow n^3 but optimal algorithm rendering degrees even.
    """
    for vertex in graph.vertices:
        if not vertex.even_degree():
            _augment_path(graph, vertex)


def make_degrees_even_fast(graph, milling_diameter):
    """
    fast approximation algorithm to obtain even degrees.
    loop on outer edge and duplicate edges between odd and even cut lines
    then loop on inner edges and duplicate edges with non valid degrees.
    bad cases appear when there is a spike between two milling levels
    or when we have even number of slices.
    """

    value, added_edges = _add_edges_in_slices(graph, milling_diameter, True)
    # cancel and try other parity
    for edge in added_edges:
        edge.remove()
    new_value, added_edges = _add_edges_in_slices(graph,
                                                  milling_diameter, False)
    if new_value > value:
        # cancel again and revert to first parity
        for edge in added_edges:
            edge.remove()
        _add_edges_in_slices(graph, milling_diameter, True)


# helper function for approx algorithm
def _add_edges_in_slices(graph, milling_diameter, slices_parity):
    """
    add edges for given parity
    """
    added_edges = []
    value = 0
    for edge in graph.frontier_edges():
        if not edge.is_almost_horizontal():
            slice_number = edge.slice_number(milling_diameter)
            if (slice_number % 2) == slices_parity:
                added_edges.append(edge)
                value += edge.path.length()
                edge.add_directly_to_graph()

    for edge in graph.get_non_oriented_edges():
        if edge.is_almost_horizontal():
            vertices = edge.vertices
            if (not vertices[0].even_degree()) and \
                    (not vertices[1].even_degree()):
                edge.add_directly_to_graph()
                value += edge.path.length()
                added_edges.append(edge)

    return (value, added_edges)


# helper functions for optimal algorithm
def _augment_path(graph, start_vertex):
    # this is a very simple way to find the best augmenting path
    # it is in no way optimized
    # and has a complexity of O(n^2)
    distances, predecessors = bellman_ford(graph, start_vertex)
    destination = _find_nearest_odd_vertex(graph, start_vertex, distances)
    current_point = destination
    if __debug__:
        if is_module_debugged(__name__):
            added_edges = []
    while current_point != start_vertex:
        edge = predecessors[current_point.unique_id]
        edge.add_directly_to_graph()
        if __debug__:
            if is_module_debugged(__name__):
                added_edges.append(edge)
        previous_point = edge.vertices[0]
        current_point = previous_point
    if __debug__:
        if is_module_debugged(__name__):
            print("new augmenting path")
            tycat(graph, added_edges)
            print("graph is now")
            tycat(graph)


def _find_nearest_odd_vertex(graph, vertex, distances):
    current_distance = float("inf")
    for destination in graph.vertices:
        distance = distances[destination.unique_id]
        if (destination.unique_id != vertex.unique_id) \
                and (current_distance > distance):
            if destination.degree() % 2:
                best_destination = destination
                current_distance = distance
    return best_destination
