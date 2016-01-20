"""
find eulerian cycle in even-degreed graph.
"""
from collections import defaultdict
from jimn.displayable import tycat
from jimn.path import Path
from jimn.utils.debug import is_module_debugged


def cycle_to_path(cycle):
    """
    converts a cycle (edges list) to a path.
    adds missing moves when needed.
    """
    paths = [e.path for e in cycle]
    return Path(paths)


def find_eulerian_cycle(graph):
    """
    eulerian cycle classical algorithm.
    requires all degrees to be even.
    """
    # we loop finding cycles until graph is empty
    possible_starts = {}  # where to search for a new cycle
    start_vertex = graph.get_any_vertex()
    # we constrain possible starting points
    # to be only a previous cycles
    # this will enable easier merging of all cycles
    possible_starts[start_vertex] = start_vertex.degree()
    # we just need to remember for each cycle its starting point
    cycle_starts = defaultdict(list)  # where do found cycles start

    first_cycle = None
    while not graph.is_empty():
        cycle = _find_cycle(graph, possible_starts)
        if __debug__:
            if is_module_debugged(__name__):
                print("found new cycle")
                tycat(graph, cycle)
        if first_cycle is None:
            first_cycle = cycle
        else:
            cycle_start = cycle[0].vertices[0]
            cycle_starts[cycle_start].append(cycle)

    final_cycle = _fuse_cycles(first_cycle, cycle_starts)
    return final_cycle


def _fuse_cycles(cycle, starts):
    """
    take a set of cycles, hashed by their starting points.
    fuse them inside given cycle.
    """
    final_edges = []
    while cycle:
        # move on ongoing path
        # reverse edges because we start from the end
        edge = cycle.pop().reverse()
        final_edges.append(edge)
        current_vertex = edge.vertices[1]
        if current_vertex in starts:
            cycles = starts[current_vertex]
            sub_cycle = cycles.pop()
            cycle.extend(sub_cycle)
            if not cycles:
                del starts[current_vertex]

    return final_edges


def _find_cycle(graph, possible_starts):
    """
    find a cycle starting from one of the possible starts.
    return a list of edges.
    """
    start_vertex = next(iter(possible_starts.keys()))
    current_vertex = start_vertex
    cycle = []
    while current_vertex.degree() != 0:

        current_edge = current_vertex.remove_any_edge()
        _update_possible_starts(graph, possible_starts, current_vertex)
        cycle.append(current_edge)

        next_vertex = current_edge.get_destination()
        next_vertex.remove_edge_to(current_vertex)
        _update_possible_starts(graph, possible_starts, next_vertex)
        current_vertex = next_vertex

    return cycle


def _update_possible_starts(graph, possible_starts, decreased_vertex):
    """
    mark if decreased_vertex is still possible a cycle starting point.
    we can still start from here if degree does not reach 0.
    else remove it from graph.
    """
    degree = decreased_vertex.degree()
    if degree:
        possible_starts[decreased_vertex] = degree
    else:
        del possible_starts[decreased_vertex]
        graph.remove_vertex(decreased_vertex)
