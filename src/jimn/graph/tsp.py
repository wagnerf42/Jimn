

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
                    tycat(g, added_edges)
            return added_edges
        e = heappop(heap)
        v1, v2 = e.get_endpoints()
        if v2 not in reached_vertices:
            added_edges.append(e)
            reached_vertices[v2] = True
            _add_edges_in_heap(heap, v2)
    raise Exception("not enough edges")

def _add_edges_in_heap(h, v):
    """
    add all edges starting at vertex v in heap h
    """
    for e in v.get_edges():
        heappush(h, e)

from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from heapq import heappush, heappop
