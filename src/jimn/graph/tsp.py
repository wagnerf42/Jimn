"""
tsp algorithms.
"""
from heapq import heappush, heappop
from collections import defaultdict
from jimn.point import Point
from jimn.displayable import tycat
from jimn.graph import Graph
from jimn.graph.edge import Edge
from jimn.graph.eulerian_cycle import find_eulerian_cycle
from jimn.graph.even_degrees import make_degrees_even
from jimn.segment import Segment
from jimn.utils.debug import is_module_debugged
from jimn.utils.points_containers import nearest_points


def tsp(graph):
    """
    christofides algorithm.
    careful: this modifies initial graph.
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("starting christofides")

    spanning_tree = min_spanning_tree(graph)
    path_graph = _adjust_degree(spanning_tree)

    cycle = find_eulerian_cycle(path_graph)
    if __debug__:
        if is_module_debugged(__name__):
            print("cycle with duplicated vertices")
            tycat(cycle)

    cycle = _skip_seen_vertices(cycle)
    if __debug__:
        if is_module_debugged(__name__):
            print("cycle")
            tycat(cycle)
    return cycle


def _adjust_degree(spanning_tree):
    """
    return graph obtained after making degrees even.
    """
    left = Graph.subgraph(_odd_degree_vertices(spanning_tree))
    make_degrees_even(left)
    if __debug__:
        if is_module_debugged(__name__):
            print("christofides : matching")
            tycat(left)

    for edge in left.double_edges():
        edge.change_multiplicity(-1)  # set multiplicity back to 1
        spanning_tree.append(edge)
    path_graph = Graph()
    for edge in spanning_tree:
        objects = [v.bound_object for v in edge.vertices]
        path_graph.add_edge_between(*objects, edge_path=edge.path)

    return path_graph


def _odd_degree_vertices(edges):
    """
    returns vertices with odd degree for given edges.
    """
    degrees = defaultdict(int)
    for edge in edges:
        for vertex in edge.vertices:
            degrees[vertex] += 1

    odd_vertices = []
    for vertex, degree in degrees.items():
        if degree % 2 == 1:
            odd_vertices.append(vertex)
    return odd_vertices


def min_spanning_tree(graph):
    """
    prim minimum spanning tree algorithm.
    """
    start = graph.get_any_vertex()
    heap = []
    _add_edges_in_heap(heap, start)

    reached_vertices = {}
    reached_vertices[start] = True
    added_edges = []

    limit = graph.vertices_number - 1  # stop after this many edges
    while heap:
        if len(added_edges) == limit:
            if __debug__:
                if is_module_debugged(__name__):
                    print("spanning tree")
                    tycat(graph, added_edges)
            return added_edges
        edge = heappop(heap)
        destination = edge.vertices[1]
        if destination not in reached_vertices:
            added_edges.append(edge)
            reached_vertices[destination] = True
            _add_edges_in_heap(heap, destination)
    print("added", len(added_edges), "need", limit)
    tycat(graph, added_edges)
    raise Exception("not enough edges")


def _add_edges_in_heap(heap, vertex):
    """
    add all edges starting at given vertex in heap.
    """
    for edge in vertex.edges:
        heappush(heap, edge)


def _skip_seen_vertices(cycle):
    """
    change cycle so that we do not pass twice at same vertex
    (except for completing the cycle).
    achieves that by shortcutting to next point using a segment.
    CAREFUL: vertices are not updated in the process.
    """
    seen_vertices = {}
    start = cycle[0].vertices[0]
    current_vertex = start
    seen_vertices[current_vertex] = True
    resulting_cycle = []
    for edge in cycle:
        next_vertex = edge.vertices[1]
        if next_vertex not in seen_vertices:
            if current_vertex == edge.vertices[0]:
                resulting_cycle.append(edge)
            else:
                objects = [
                    v.bound_object for v in (current_vertex, next_vertex)
                ]
                if isinstance(objects[0], Point) \
                        and isinstance(objects[0], Point):
                    point1, point2 = objects
                else:
                    point1, point2 = nearest_points(objects[0], objects[1])

                resulting_cycle.append(
                    Edge(current_vertex, next_vertex, Segment([point1, point2]))
                )
            current_vertex = next_vertex
            seen_vertices[current_vertex] = True

    # add last segment to go back at start
    resulting_cycle.append(cycle[-1])
    return resulting_cycle
