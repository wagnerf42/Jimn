from jimn.displayable import tycat
from jimn.path import path
from jimn.utils.debug import is_module_debugged
from collections import defaultdict


def cycle_to_path(c):
    """
    converts a cycle to a path.
    adds missing moves when needed.
    """
    paths = [e.get_path() for e in c]
    return path(paths)

def find_eulerian_cycle(g):
    """
    eulerian cycle classical algorithm.
    requires all degrees to be even.
    """
    # we loop finding cycles until graph is empty
    possible_starts = {}  # where to search for a new cycle
    start_vertex = g.get_any_vertex()
    # we constrain possible starting points
    # to be only a previous cycles
    # this will enable easier merging of all cycles
    possible_starts[start_vertex] = start_vertex.degree()
    # we just need to remember for each cycle its starting point
    cycle_starts = defaultdict(list)  # where do found cycles start

    first_cycle = None
    while not g.is_empty():
        c = _find_cycle(g, possible_starts)
        if __debug__:
            if is_module_debugged(__name__):
                print("found new cycle")
                tycat(g, c)
        if first_cycle is None:
            first_cycle = c
        else:
            cycle_start = c[0].get_endpoint(0)
            cycle_starts[cycle_start].append(c)

    final_cycle = _fuse_cycles(first_cycle, cycle_starts)
    return final_cycle

def _fuse_cycles(cycle, starts):
    """
    takes a set of cycles, hashed by their starting points.
    fuses them inside given cycle.
    """
    final_edges = []
    while cycle:
        # move on ongoing path
        # reverse edges because we start from the end
        e = cycle.pop().reverse()
        final_edges.append(e)
        current_vertex = e.get_endpoint(1)
        if current_vertex in starts:
            cycles = starts[current_vertex]
            sub_cycle = cycles.pop()
            cycle.extend(sub_cycle)
            if not cycles:
                del starts[current_vertex]

    return final_edges


def _find_cycle(g, possible_starts):
    """
    finds a cycle starting from one of the possible starts.
    returns a list of edges.
    """
    start_vertex = next(iter(possible_starts.keys()))
    current_vertex = start_vertex
    cycle = []
    while current_vertex.degree() != 0:

        current_edge = current_vertex.remove_any_edge()
        _update_possible_starts(g, possible_starts, current_vertex)
        cycle.append(current_edge)

        next_vertex = current_edge.get_destination()
        next_vertex.remove_edge_to(current_vertex)
        _update_possible_starts(g, possible_starts, next_vertex)
        current_vertex = next_vertex

    return cycle


def _update_possible_starts(g, possible_starts, decreased_vertex):
    """
    we can still start from here if degree does not reach 0
    """
    degree = decreased_vertex.degree()
    if degree:
        possible_starts[decreased_vertex] = degree
    else:
        del possible_starts[decreased_vertex]
        g.remove_vertex(decreased_vertex)
