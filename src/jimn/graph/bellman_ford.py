from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged

def bellman_ford(searched_graph, source_vertex):
    """
    bellman ford shortest path algorithm.
    assumes no negative cycles
    """
    g = searched_graph
    source_id = source_vertex.id
    predecessors = [None for i in range(g.get_max_vertices_number())]
    distances = [float("inf") for i in range(g.get_max_vertices_number())]
    # init
    distances[source_vertex.id] = 0
    for e in source_vertex.get_edges():
        destination = e.get_destination()
        predecessors[destination.id] = e
        w = e.get_weight()
        distances[destination.id] = w

    # extract a maximum of operations out of main loop
    # precompute all weights and ids
    edges = []
    for e in g.get_all_edges():
        v1, v2 = [p.id for p in e.get_endpoints()]
        w = e.get_weight()
        edges.append((v1, v2, w, e))

    # go
    for useless in range(g.get_vertices_number()-1):
        for v1, v2, w, e in edges:
            new_distance = distances[v1] + w
            if distances[v2] > new_distance:
                if _not_in_incoming_path(predecessors, source_id, v1, v2):
                    predecessors[v2] = e
                    distances[v2] = new_distance
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
        e = predecessors[current_vertex]
        current_vertex = e.vertices[0].id
    return True
