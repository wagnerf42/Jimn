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
    for e in g.get_edges_from(source_vertex):
        destination = e.get_destination()
        predecessors[destination] = e
        w = e.get_weight()
        distances[destination] = w
    # go
    for useless in range(g.get_vertices_number()-1):
        for e in g.get_all_edges():
            v1, v2 = e.get_endpoints()
            # skip going directly back
            if (v1 not in predecessors) or \
                    (predecessors[v1].get_endpoint(0) != v2):
                w = e.get_weight()
                new_distance = distances[v1] + w
                if distances[v2] > new_distance:
                    predecessors[v2] = e
                    distances[v2] = new_distance
    if __debug__:
        if is_module_debugged(__name__):
            print("bellman ford : result")
            tycat(g, source_vertex, list(predecessors.values()))
    return (distances, predecessors)
