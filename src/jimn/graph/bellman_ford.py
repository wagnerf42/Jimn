from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged


def bellman_ford(searched_graph, source_vertex):
    """
    bellman ford shortest path algorithm.
    assumes no negative cycles
    """
    g = searched_graph
    predecessors = [None for i in range(g.get_max_vertices_number())]
    distances = [float("inf") for i in range(g.get_max_vertices_number())]
    # init
    distances[source_vertex.get_id()] = 0
    for e in source_vertex.get_edges():
        destination = e.get_destination()
        predecessors[destination.get_id()] = e
        w = e.get_weight()
        distances[destination.get_id()] = w
    # go
    for useless in range(g.get_vertices_number()-1):
        for e in g.get_all_edges():
            v1, v2 = e.get_endpoints()
            w = e.get_weight()
            new_distance = distances[v1.get_id()] + w
            if distances[v2.get_id()] > new_distance:
                if _not_in_incoming_path(predecessors, source_vertex, v1, v2):
                    predecessors[v2.get_id()] = e
                    distances[v2.get_id()] = new_distance
    if __debug__:
        if is_module_debugged(__name__):
            print("bellman ford : result")
            real_predecessors = [p for p in predecessors if p is not None]
            tycat(g, source_vertex, real_predecessors)
    return (distances, predecessors)


def _not_in_incoming_path(predecessors, start, end, to_avoid):
    """
    return true if 'to_avoid' vertex is not between start and end
    """
    current_vertex = end
    while current_vertex != start:
        if current_vertex == to_avoid:
            return False
        e = predecessors[current_vertex.get_id()]
        assert current_vertex == e.get_endpoint(1)
        current_vertex = e.get_endpoint(0)
    return True
