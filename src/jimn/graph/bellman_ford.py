"""
bellman ford shortest paths algorithm.
"""
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged


def bellman_ford(graph, source_vertex):
    """
    bellman ford shortest path algorithm.
    assumes no negative cycles.
    """
    source_id = source_vertex.unique_id
    predecessors = [None for i in range(graph.get_max_vertices_number())]
    distances = [float("inf") for i in range(graph.get_max_vertices_number())]
    # init
    distances[source_vertex.unique_id] = 0
    for edge in source_vertex.edges:
        destination = edge.get_destination()
        predecessors[destination.unique_id] = edge
        distances[destination.unique_id] = edge.weight

    # extract a maximum of operations out of main loop
    # precompute all weights and ids
    edges = []
    for edge in graph.get_all_edges():
        vertex_id1, vertex_id2 = [p.unique_id for p in edge.vertices]
        edges.append((vertex_id1, vertex_id2, edge.weight, edge))

    # go
    for _ in range(graph.vertices_number-1):
        for vertex_id1, vertex_id2, weight, edge in edges:
            new_distance = distances[vertex_id1] + weight
            if distances[vertex_id2] > new_distance:
                if _not_in_incoming_path(predecessors, source_id,
                                         vertex_id1, vertex_id2):
                    predecessors[vertex_id2] = edge
                    distances[vertex_id2] = new_distance
    if __debug__:
        if is_module_debugged(__name__):
            print("bellman ford : result")
            real_predecessors = [p for p in predecessors if p is not None]
            tycat(graph, source_vertex, real_predecessors)
    return (distances, predecessors)


def _not_in_incoming_path(predecessors, start, end, to_avoid):
    """
    return true if 'to_avoid' vertex is not between start and end
    """
    current_vertex = end
    while current_vertex != start:
        if current_vertex == to_avoid:
            return False
        edge = predecessors[current_vertex]
        current_vertex = edge.vertices[0].unique_id
    return True
