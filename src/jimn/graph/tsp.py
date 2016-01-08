

def tsp(g):
    """
    christofides algorithm.
    careful: this modifies initial graph.
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("starting christofides")

    st = min_spanning_tree(g)

    left = g.subgraph(_odd_degree_vertices(st))
    make_degrees_even(left)
    if __debug__:
        if is_module_debugged(__name__):
            print("christofides : matching")
            tycat(left)

    for e in left.get_double_edges():
        e.change_multiplicity(-1)  # set multiplicity back to 1
        st.append(e)
    path_graph = graph()
    for e in st:
        objects = [v.get_object() for v in e.get_endpoints()]
        path_graph.add_edge_between(*objects, edge_path=e.get_path())

    c = find_eulerian_cycle(path_graph)
    if __debug__:
        if is_module_debugged(__name__):
            print("cycle with duplicated vertices")
            tycat(c)

    c = _skip_seen_vertices(c)
    if __debug__:
        if is_module_debugged(__name__):
            print("cycle")
            tycat(c)
    return c


def _odd_degree_vertices(edges):
    """
    returns vertices with odd degree for given edges.
    """
    c = defaultdict(int)
    for e in edges:
        v1, v2 = e.get_endpoints()
        c[v1] += 1
        c[v2] += 1

    odd_vertices = []
    for v, count in c.items():
        if count % 2 == 1:
            odd_vertices.append(v)
    return odd_vertices


def min_spanning_tree(g):
    """
    prim minimum spanning tree algorithm.
    """
    start = g.get_any_vertex()
    heap = []
    _add_edges_in_heap(heap, start)

    reached_vertices = {}
    reached_vertices[start] = True
    added_edges = []

    limit = g.get_vertices_number() - 1  # stop after this many edges
    while heap:
        if len(added_edges) == limit:
            if __debug__:
                if is_module_debugged(__name__):
                    print("spanning tree")
                    tycat(g, added_edges)
            return added_edges
        e = heappop(heap)
        v1, v2 = e.get_endpoints()
        if v2 not in reached_vertices:
            added_edges.append(e)
            reached_vertices[v2] = True
            _add_edges_in_heap(heap, v2)
    print("added", len(added_edges), "need", limit)
    tycat(g, added_edges)
    raise Exception("not enough edges")


def _add_edges_in_heap(h, v):
    """
    add all edges starting at vertex v in heap h
    """
    for e in v.get_edges():
        heappush(h, e)


def _skip_seen_vertices(cycle):
    """
    changes cycle so that we do not pass twice at same vertex
    (except for completing the cycle).
    achieves that by shortcutting to next point using a segment.
    CAREFUL: vertices are not updated in the process.
    """
    seen_vertices = {}
    start = cycle[0].vertices[0]
    current_vertex = start
    seen_vertices[current_vertex] = True
    resulting_cycle = []
    for e in cycle:
        next_vertex = e.vertices[1]
        if next_vertex not in seen_vertices:
            if current_vertex == e.vertices[0]:
                resulting_cycle.append(e)
            else:
                objects = [
                    v.get_object() for v in (current_vertex, next_vertex)
                ]
                if isinstance(objects[0], Point) \
                        and isinstance(objects[0], Point):
                    p1, p2 = objects
                else:
                    p1, p2 = nearest_points(objects[0], objects[1])

                resulting_cycle.append(
                    edge(current_vertex, next_vertex, Segment([p1, p2]))
                )
            current_vertex = next_vertex
            seen_vertices[current_vertex] = True

    # add last segment to go back at start
    resulting_cycle.append(cycle[-1])
    return resulting_cycle


from jimn.point import Point
from jimn.displayable import tycat
from jimn.graph import graph
from jimn.graph.edge import edge
from jimn.graph.eulerian_cycle import find_eulerian_cycle
from jimn.graph.even_degrees import make_degrees_even
from jimn.segment import Segment
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements
from jimn.utils.points_containers import nearest_points
from heapq import heappush, heappop
from collections import defaultdict
