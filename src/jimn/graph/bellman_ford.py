from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from collections import defaultdict


def bellman_ford(searched_graph, source_vertex):
    """
    bellman ford shortest path algorithm.
    assumes no negative cycles
    """
    g = searched_graph
    predecessors = {}
    distances = defaultdict(lambda: float("inf"))
    # init
    distances[source_vertex] = 0
    for e in source_vertex.get_edges():
        destination = e.get_destination()
        predecessors[destination] = e
        w = e.get_weight()
        distances[destination] = w
    # go
    for useless in range(g.get_vertices_number()-1):
        for e in g.get_all_edges():
            v1, v2 = e.get_endpoints()
            w = e.get_weight()
            new_distance = distances[v1] + w
            if distances[v2] > new_distance:
                if _not_in_incoming_path(predecessors, source_vertex, v1, v2):
                    predecessors[v2] = e
                    distances[v2] = new_distance
    if __debug__:
        if is_module_debugged(__name__):
            print("bellman ford : result")
            tycat(g, source_vertex, list(predecessors.values()))
    return (distances, predecessors)


def _not_in_incoming_path(predecessors, start, end, to_avoid):
    """
    return true if 'to_avoid' vertex is not between start and end
    """
    current_vertex = end
    while current_vertex != start:
        if current_vertex == to_avoid:
            return False
        e = predecessors[current_vertex]
        assert current_vertex == e.get_endpoint(1)
        current_vertex = e.get_endpoint(0)
    return True
